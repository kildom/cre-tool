
import cre from '../con-reg-exp/src/con-reg-exp';
import * as fs from 'node:fs';
import * as path from 'node:path';
import * as acorn from 'acorn';
import { globSync, Path } from 'glob';
import packageJson from '../package.json';
import { Command } from 'commander';

type Dict<T> = { [key: string]: T };

interface GeneratedTemplateStringsArray extends Array<string> {
    raw: string[];
}


const program = new Command();

program
    .name(packageJson.name)
    .description(packageJson.description)
    .version(packageJson.version);

program.command('compile')
    .description('Compile CRE expressions embedded in comments.')
    .argument('<file-or-glob...>', 'Files to compile.')
    .option('-s, --silent', 'Do not display summary information.')
    .action((globPatterns, options) => {
        compile(globPatterns, false, options.silent);
    });

program.command('verify')
    .description('Check if CRE expressions embedded in comments are matching the code.')
    .argument('<file-or-glob...>', 'Files to check.')
    .option('-s, --silent', 'Do not display summary information.')
    .action((globPatterns, options) => {
        compile(globPatterns, true, options.silent);
    });

interface Whitespace {
    mandatory: RegExp;
    optional: RegExp;
};

const whitespace = {
    mandatory: cre`at-least-1 [ \t];`,
    optional: cre`repeat [ \t];`,
};

const whitespaceWithNewLine = {
    mandatory: cre`at-least-1 [ \t\r\n];`,
    optional: cre`repeat [ \t\r\n];`,
};

const crePattern = (ws: Whitespace) => cre`
    ${ws.optional};
    "cre";
    lookahead not {
        ${ws.optional};
        ".";
        ${ws.optional};
        "import";
        ${ws.mandatory};
    }
    Flags: repeat {
        ${ws.optional};
        ".";
        ${ws.optional};
        at-least-1 [a-zA-Z];
    }
    ${ws.optional};
    "\`";
    Text: lazy-repeat any;
    ${ws.optional};
`;

const expressionPattern = cre.global.indices`
    prefix<sl> {
        "//";
        ${crePattern(whitespace)};
        end-of-line;
    } or prefix<ml> {
        "/*";
        ${crePattern(whitespaceWithNewLine)};
        "*/";
    }
    repeat {
        [\r\n\t ]
        or {
            "/*";
            lazy-repeat any;
            "*/";
        } or {
            "//";
            lazy-repeat any;
            end-of-line;
        }
    }
    code: lazy-repeat any;
    end-of-line;
`;

interface ExpressionGroups {
    slFlags?: string;
    slText?: string;
    mlFlags?: string;
    mlText?: string;
    code: string;
}

type ExpressionIndices = { [key in keyof ExpressionGroups]: [number, number] | undefined };

const definitionPatternInner = (ws: Whitespace) => cre`
    ${ws.optional};
    Identifier: at-least-1 [a-zA-Z0-9_$.];
    ${ws.optional};
    "=";
    ${crePattern(ws)};
`;


const definitionPattern = cre.global.indices`
    prefix<sl> {
        "//";
        ${definitionPatternInner(whitespace)};
        end-of-line;
    } or prefix<ml> {
        "/*";
        ${definitionPatternInner(whitespaceWithNewLine)};
        "*/";
    }
`;

interface DefinitionGroups {
    slIdentifier?: string;
    slFlags?: string;
    slText?: string;
    mlIdentifier?: string;
    mlFlags?: string;
    mlText?: string;
}

type DefinitionIndices = { [key in keyof DefinitionGroups]: [number, number] | undefined };

const importPatternInner = (ws: Whitespace) => cre`
    ${ws.optional};
    "cre";
    ${ws.optional};
    ".";
    Code: {
        ${ws.optional};
        "import";
        ${ws.mandatory};
        lazy-repeat any;
    }
`;

const importPattern = cre.global.indices`
    prefix<sl> {
        "//";
        ${importPatternInner(whitespace)};
        end-of-line;
    } or prefix<ml> {
        "/*";
        ${importPatternInner(whitespaceWithNewLine)};
        "*/";
    }
`;

interface ImportGroups {
    slCode?: string;
    mlCode?: string;
}

interface Definition {
    identifier: string;
    flags: string[];
    expression: string;
    start: number;
    end: number;
    expressionStart: number;
}

interface Expression {
    before: string;
    flags: string[];
    expression: string;
    oldPattern: string;
    after: string;
    start: number;
    end: number;
    expressionStart: number;
}

interface Import {
    code: string;
    start: number;
    end: number;
}

class Module {
    public namespace: Dict<RegExp | undefined> = Object.create(null);
    public parsing: boolean = true;
    constructor(public file: string) { }
}

function dict<T>(): Dict<T> {
    return Object.create(null) as Dict<T>;
}

const allowedFlags: Dict<true> = {
    indices: true,
    global: true,
    ignoreCase: true,
    legacy: true,
    unicode: true,
    sticky: true,
};

class Compiler {

    public stats = {
        failed: false,
        errors: 0,
        definitions: 0,
        expressions: 0,
        expressionsModified: 0,
        filesModified: 0,
    };

    private modules = dict<Module>();
    private fileNameStack: string[] = [];
    private fileTextStack: string[] = [];
    private fileModuleStack: Module[] = [];

    private fileName?: string;
    private fileText?: string;
    private fileModule?: Module;

    private error(location: number, message: string): void {
        let sub = (this.fileText || '').substring(0, location);
        let lines = sub.split(/\n/g);
        let lineNumber = lines.length;
        let columnNumber = (lines.at(-1)?.length || 0) + 1;
        console.error(`${this.fileName || 'unknown'}:${lineNumber}:${columnNumber}: error: ${message}`);
        this.stats.failed = true;
        this.stats.errors++;
    }

    private pushFile(fileName: string, fileText: string, fileModule: Module): void {
        this.fileNameStack.push(fileName);
        this.fileTextStack.push(fileText);
        this.fileModuleStack.push(fileModule);
        this.fileName = fileName;
        this.fileText = fileText;
        this.fileModule = fileModule;
    }

    private popFile() {
        this.fileNameStack.pop();
        this.fileTextStack.pop();
        this.fileModuleStack.pop();
        this.fileName = this.fileNameStack.at(-1);
        this.fileText = this.fileTextStack.at(-1);
        this.fileModule = this.fileModuleStack.at(-1);
    }

    private getFullFileName(file: string | Path): string {
        if (typeof file === 'string') {
            return fs.realpathSync(file);
        } else {
            return fs.realpathSync(file.fullpath());
        }
    }

    public getModule(fileName: string, text?: string): Module {

        let result: Module;

        if (this.modules[fileName]) {
            result = this.modules[fileName];
            if (result.parsing) {
                this.pushFile(fileName, '', result);
                this.error(0, 'Circular dependency detected.');
                this.popFile();
            }
            return result;
        }

        result = new Module(fileName);
        this.modules[fileName] = result;

        if (text === undefined) {
            text = fs.readFileSync(fileName, 'utf-8');
        }

        this.pushFile(fileName, text, result);

        for (let importEntry of this.parseImports(text)) {
            this.importToNamespace(importEntry);
        }
        for (let def of this.parseDefinitions(text)) {
            if (result.namespace[def.identifier]) {
                this.error(def.start, 'Already defined.');
            }
            let re = this.compileExpression(def.expression, def.flags, def.expressionStart);
            result.namespace[def.identifier] = re;
        }

        this.popFile();
        result.parsing = false;

        return result;
    }

    private acornParse(code: string, locationOffset: number): acorn.Program | undefined {
        try {
            return acorn.parse(code, {
                ecmaVersion: 2015,
                sourceType: 'module',
            });
        } catch (err) {
            if (err instanceof SyntaxError) {
                this.error(locationOffset + ((err as any)?.pos || 0), 'Syntax error: ' + err.message);
                return undefined;
            } else {
                throw err;
            }
        }
    }

    private importToNamespace(importEntry: Import) {
        let root = this.acornParse(importEntry.code, importEntry.start);
        if (!root || root.type !== 'Program' || root.body.length !== 1 || root.body[0].type !== 'ImportDeclaration') {
            this.error(importEntry.start, 'Invalid import statement.');
            return;
        }
        let decl = root.body[0] as acorn.ImportDeclaration;
        if (decl.source.type !== 'Literal' || typeof decl.source.value !== 'string') {
            this.error(importEntry.start, 'Invalid import statement.');
            return;
        }
        let baseDir = path.dirname(this.fileName || './x');
        let importFileName = this.getFullFileName(path.join(baseDir, decl.source.value as string));
        let module = this.getModule(importFileName);
        for (let spec of decl.specifiers) {
            if (spec.local.type !== 'Identifier') {
                this.error(importEntry.start, 'Invalid import specifier.');
                continue;
            }
            let localName = spec.local.name;
            if (spec.type === 'ImportSpecifier') {
                if (spec.imported.type !== 'Identifier') {
                    this.error(importEntry.start, 'Invalid import specifier.');
                    continue;
                }
                let remoteName = spec.imported.name;
                let failed = true;
                if (module.namespace[remoteName]) {
                    this.fileModule!.namespace[localName] = module.namespace[remoteName];
                    failed = false;
                }
                for (let [key, value] of Object.entries(module.namespace)) {
                    if (key.startsWith(remoteName + '.')) {
                        this.fileModule!.namespace[localName + key.substring(remoteName.length)] = value;
                        failed = false;
                    }
                }
                if (failed) {
                    this.error(importEntry.start, 'Import specifier does not found anything to import.');
                    continue;
                }
            } else if (spec.type === 'ImportNamespaceSpecifier') {
                for (let [key, value] of Object.entries(module.namespace)) {
                    this.fileModule!.namespace[localName + '.' + key] = value;
                }
            } else if (spec.type === 'ImportDefaultSpecifier') {
                this.error(importEntry.start, 'Cannot use default import.');
                continue;
            } else {
                this.error(importEntry.start, 'Invalid import statement.');
                continue;
            }
        }
    }

    private compileExpression(expression: string, flags: string[], location: number): RegExp | undefined {
        let locationOffset = location - 1;
        let code = '`' + expression + '`';
        let root = this.acornParse(code, locationOffset);
        if (!root || root.type !== 'Program' || root.body.length !== 1 || root.body[0].type !== 'ExpressionStatement') {
            this.error(location, 'Invalid expression.');
            return undefined;
        }
        let stmt = root.body[0] as acorn.ExpressionStatement;
        if (stmt.expression.type !== 'TemplateLiteral') {
            this.error(location, 'Invalid expression.');
            return undefined;
        }
        let str = [] as unknown as GeneratedTemplateStringsArray;
        str.raw = [];
        for (let element of stmt.expression.quasis) {
            if (element.type !== 'TemplateElement') {
                this.error(location, 'Invalid expression.');
                return undefined;
            }
            str.push(element.value.cooked as string);
            str.raw.push(element.value.raw);
        }
        let values: RegExp[] = [];
        for (let element of stmt.expression.expressions) {
            let id = this.memberExpressionToString(element, locationOffset);
            if (!id) {
                return undefined;
            } else if (!this.fileModule?.namespace[id]) {
                this.error(locationOffset + element.start, `Undefined name "${id}".`);
                return undefined;
            }
            values.push(this.fileModule?.namespace[id] as RegExp);
        }
        let creFunction = cre;
        for (let flag of flags) {
            if (!allowedFlags[flag]) {
                this.error(location, `Invalid flag "${flag}".`);
                return undefined;
            }
            creFunction = (creFunction as any)[flag];
        }
        try {
            return creFunction(str, ...values);
        } catch (err) {
            if (err instanceof cre.Error) {
                this.error(location, 'Error in expression: ' + err.message);
            } else {
                throw err;
            }
        }
    }

    private memberExpressionToString(expression: acorn.Expression | acorn.Super, locationOffset: number): string | undefined {
        if (expression.type === 'Identifier') {
            return expression.name;
        } else if (expression.type === 'MemberExpression') {
            if (expression.computed || expression.property.type !== 'Identifier') {
                this.error(locationOffset + expression.start, 'Invalid interpolated value.');
                return undefined;
            }
            return this.memberExpressionToString(expression.object, locationOffset) + '.' + expression.property.name;
        } else {
            this.error(locationOffset + expression.start, 'Invalid interpolated value.');
            return undefined;
        }
    }
    
    private parseExpressions(text: string): Expression[] {
        let expressions: Expression[] = [];
        for (let m of text.matchAll(expressionPattern)) {
            let groups = m.groups as unknown as ExpressionGroups;
            let indices = (m as any).indices.groups as unknown as ExpressionIndices;
            let start: number = (m as any).indices[0][0];
            let end: number = (m as any).indices[0][1];
            if (expressions.length > 0) {
                expressions.at(-1)!.after += text.substring(expressions.at(-1)!.end, start);
            }
            let from = groups.code.indexOf('/');
            let to = groups.code.lastIndexOf('/');
            if (from === to) {
                this.error(indices.code![0], 'Cannot recognize beginning or ending of the target regular expression.');
                continue;
            }
            let codeBefore = groups.code.substring(0, from);
            let codeAfter = groups.code.substring(to + 1);
            let codeFlags: string = (codeAfter.match(/^[dgimsuvy]*/) as any)[0];
            codeAfter = codeAfter.substring(codeFlags.length);
            expressions.push({
                flags: (groups.mlFlags || groups.slFlags || '')!.split('.').map(x => x.trim()).filter(x => x),
                before: text.substring((expressions.length == 0) ? 0 : start, indices.code![0]) + codeBefore,
                expression: this.cutEnding(groups.mlText || groups.slText, (indices.mlText || indices.slText)![1]),
                oldPattern: groups.code.substring(from, groups.code.length - codeAfter.length),
                after: codeAfter,
                start,
                end,
                expressionStart: (indices.mlText || indices.slText)![0],
            });
            this.stats.expressions++;
        }
        if (expressions.length > 0) {
            expressions.at(-1)!.after += text.substring(expressions.at(-1)!.end);
        }
        return expressions;
    }

    private parseDefinitions(text: string): Definition[] {
        let definitions: Definition[] = [];
        for (let m of text.matchAll(definitionPattern)) {
            let groups = m.groups as unknown as DefinitionGroups;
            let indices = (m as any).indices.groups as unknown as DefinitionIndices;
            let start: number = (m as any).indices[0][0];
            let end: number = (m as any).indices[0][1];
            definitions.push({
                identifier: (groups.mlIdentifier || groups.slIdentifier) as string,
                flags: (groups.mlFlags || groups.slFlags || '')!.split('.').map(x => x.trim()).filter(x => x),
                expression: this.cutEnding(groups.mlText || groups.slText, (indices.mlText || indices.slText)![1]) as string,
                start,
                end,
                expressionStart: (indices.mlText || indices.slText)![0],
            });
            this.stats.definitions++;
        }
        return definitions;
    }

    private cutEnding(text: string | undefined, location: number): string {
        text = text || '';
        let m = text.match(/`\s*;?\s*$/);
        if (m) {
            return text.substring(0, text.length - m[0].length);
        } else {
            this.error(location, 'Unterminated expression. Use "`" character.');
            return '';
        }
    }

    private parseImports(text: string): Import[] {
        let imports: Import[] = [];
        for (let m of text.matchAll(importPattern)) {
            let groups = m.groups as unknown as ImportGroups;
            let start: number = (m as any).indices[0][0];
            let end: number = (m as any).indices[0][1];
            imports.push({
                code: (groups.mlCode || groups.slCode) as string,
                start,
                end,
            });
        }
        return imports;
    }

    public compile(fileName: string): string | undefined {
        fileName = this.getFullFileName(fileName);
        let text = fs.readFileSync(fileName, 'utf-8');
        let expressions = this.parseExpressions(text);
        if (expressions.length === 0) {
            return undefined;
        }
        let module = this.getModule(fileName, text) || new Module(fileName);
        this.pushFile(fileName, text, module);
        let output: string[] = [];
        for (let exp of expressions) {
            output.push(exp.before);
            let re = this.compileExpression(exp.expression, exp.flags, exp.expressionStart);
            let newPattern = `${re || '/? ERROR ?/'}`;
            if (exp.oldPattern !== newPattern) {
                this.stats.expressionsModified++;
            }
            output.push(newPattern);
            output.push(exp.after);
        }
        this.popFile();
        let textOutput = output.join('');
        if (textOutput !== text) {
            this.stats.filesModified++;
            return textOutput;
        } else {
            return undefined;
        }
    }

    public verify(fileName: string): boolean {
        fileName = this.getFullFileName(fileName);
        let text = fs.readFileSync(fileName, 'utf-8');
        let module = this.getModule(fileName, text) || new Module(fileName);
        this.pushFile(fileName, text, module);
        let expressions = this.parseExpressions(text);
        for (let exp of expressions) {
            let re = this.compileExpression(exp.expression, exp.flags, exp.expressionStart);
            let newPattern = `${re || '/? ERROR ?/'}`;
            if (exp.oldPattern !== newPattern) {
                if (newPattern === '/? ERROR ?/') {
                    this.error(exp.end, 'Verification failed.');
                } else {
                    this.error(exp.end, `Verification failed\n    expected: ${newPattern}\n    got:      ${exp.oldPattern}`);
                }
            }
        }
        this.popFile();
        return !this.stats.failed;
    }

}

function createFileList(globPatterns: string[]) {
    let files = new Set<string>();
    for (let p of globPatterns) {
        let remove = p.startsWith('!');
        if (remove) {
            p = p.substring(1);
        }
        for (let file of globSync(p, { windowsPathsNoEscape: true, withFileTypes: true })) {
            if (!file.isFile()) {
                continue;
            }
            let fullPath = file.realpathSync()?.fullpath();
            if (fullPath) {
                if (!remove) {
                    files.add(fullPath);
                } else if (files.has(fullPath)) {
                    files.delete(fullPath);
                }
            }
        }
    }
    return [...files];
}

function compile(globPatterns: string[], verify: boolean, silent: boolean) {
    let files = createFileList(globPatterns);
    let compiler = new Compiler();
    for (let file of files) {
        if (verify) {
            compiler.verify(file);
        } else {
            let out = compiler.compile(file);
            if (out !== undefined) {
                fs.writeFileSync(file, out);
            }
        }
    }
    if (!silent) {
        console.log('Errors:              ', compiler.stats.errors);
        console.log('Definitions:         ', compiler.stats.definitions);
        console.log('Expressions:         ', compiler.stats.expressions);
        if (!verify) {
            console.log('Expressions Modified:', compiler.stats.expressionsModified);
            console.log('Files Modified:      ', compiler.stats.filesModified);
        }
    }
    if (compiler.stats.failed) {
        process.exitCode = 1;
    } else if (!silent) {
        console.log(verify ? 'Verification successful.' : 'Compilation successful.');
    }
}

program.parse();

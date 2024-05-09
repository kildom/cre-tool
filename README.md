# CLI tool for [Convenient Regular Expressions](https://kildom.github.io/con-reg-exp/)

With this command line tool, you can compile your [Convenient Regular Expressions](https://kildom.github.io/con-reg-exp/)
ahead of time. You can put the `cre` expression in a comment and
this tool will generate the standard JavaScript regular expression below.

Install it with:

```
npm install --save-dev cre-tool
```

For example, the code:

```javascript
// cre` "Hello World" `
const myRegex = / /;
```

after running:

```
cre-tool compile '**/*.js'
```

becomes:

```javascript
// cre` "Hello World" `
const myRegex = /Hello World/su;
```

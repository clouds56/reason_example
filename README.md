# reduce_issue

## setup

```
bsb -init reduce_issue -theme react

# modify bsconfig.json
"refmt": 3,

# modify package.json
"bs-platform": "^2.0.0",

```

Run this project:

```
npm link bs-platform
npm install
npm start
# in another tab
npm run webpack
```

After you see the webpack compilation succeed (the `npm run webpack` step), open up the nested html files in `src/*` (**no server needed!**). Then modify whichever file in `src` and refresh the page to see the changes.

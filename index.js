require('./index.css');
if (process.env.NODE_ENV == "production") {
  require("./dist/Main").main();
} else {
  require("/output/Main").main();
}

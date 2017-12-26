const path = require('path');
const BUILD_DIR = path.resolve(__dirname, 'build');
const SRC_DIR = path.resolve(__dirname, 'src');

var config = {
  entry: path.join(SRC_DIR, 'index.bs.js'),
  output: {
    path: BUILD_DIR,
    filename: 'index.js',
  },
  module : {
    loaders : [
      {
        test : /\.jsx?/,
        include : SRC_DIR,
        loader : 'babel-loader'
      }
    ]
  }
};
module.exports = config;

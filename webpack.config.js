const Dotenv = require('dotenv-webpack');
const path = require('path');

module.exports = {
  entry: './entry.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'app.js',
  },
  plugins: [
    new Dotenv(),
  ],
  watch: true,
};

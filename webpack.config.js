const path = require('path');
const Dotenv = require('dotenv-webpack');

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

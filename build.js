require('dotenv').config();

const browserify = require('browserify');
const fs = require('fs');

const b = browserify(__dirname + '/output/index.js');
const output = fs.createWriteStream(__dirname + '/dist/app.js');

b.transform('envify');
b.transform('uglifyify', { global: true });
b.bundle().pipe(output);

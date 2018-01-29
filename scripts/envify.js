require('dotenv').config();
const b = require('browserify')(__dirname + '/../output/index.js');
const output = require('fs').createWriteStream(__dirname + '/../dist/app.js');
b.transform('envify');
b.bundle().pipe(output);

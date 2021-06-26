const esbuild = require('esbuild');
const fs = require('fs');
const fsp = require('fs/promises');
const path = require('path');

const buildOptions = {
  entryPoints: [
    './src/js/app.js',
    './src/css/styles.css'
  ],
  outdir: './dist/assets/',
  sourcemap: false,
  minify: true,
  bundle: true
}

fs.rmdirSync('./dist', { recursive: true });
fs.mkdirSync('./dist');

fsp.copyFile('./src/html/index.html', './dist/index.html');
copyDir('./src/img', './dist/assets/img', [ 'webp', 'ico' ]);

if (process.argv.includes('--serve')) {
  buildOptions.sourcemap = true;

  esbuild.serve({ servedir: 'dist' }, buildOptions)
    .then(server => {
      console.log('Listening on ' + server.host + ':' + server.port);
      const start = (process.platform == 'darwin' ? 'open': process.platform == 'win32' ? 'start' : 'xdg-open');
      require('child_process').exec(start + ' http://' + server.host + ':' + server.port);
    });
} else {
  esbuild.buildSync(buildOptions);
}

function copyDir(src, dest, filetypes) {
  fs.mkdirSync(dest, { recursive: true });
  let entries = fs.readdirSync(src, { withFileTypes: true });

  for (let entry of entries) {
    let srcPath = path.join(src, entry.name);
    let destPath = path.join(dest, entry.name);

    if (entry.isDirectory()) {
      copyDir(srcPath, destPath, filetypes);
    } else if (filetypes.includes(entry.name.split('.').pop().toLowerCase())) {
      fsp.copyFile(srcPath, destPath);
    }
  }
}

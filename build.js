const esbuild = require('esbuild');
const fs = require('fs');
const fsp = require('fs/promises');
const path = require('path');

const buildOptions = {
  entryPoints: [
    './src/js/app.js'
    , './src/css/styles.css'
  ]
  , outdir: './dist/assets/'
  , sourcemap: false
  , minify: true
  , bundle: true
};

fs.rmdirSync('./dist', {recursive: true});
fs.mkdirSync('./dist');

fs.closeSync(fs.openSync('./dist/.nojekyll', 'w'));
fsp.copyFile('./src/html/index.html', './dist/index.html');
copyDir('./src/img', './dist/assets/img', ['webp', 'ico']);

if (process.argv.includes('--serve')) {
  buildOptions.sourcemap = true;

  esbuild.serve({servedir: 'dist'}, buildOptions)
    .then(server => {
      console.log('Listening on ' + server.host + ':' + server.port);
      require('child_process').exec(getStartCommand(process.platform) + ' http://' + server.host + ':' + server.port);
    });
} else {
  esbuild.buildSync(buildOptions);
}

function copyDir(src, dest, filetypes) {
  fs.mkdirSync(dest, {recursive: true});
  const entries = fs.readdirSync(src, {withFileTypes: true});

  for (const entry of entries) {
    const srcPath = path.join(src, entry.name);
    const destPath = path.join(dest, entry.name);

    if (entry.isDirectory()) {
      copyDir(srcPath, destPath, filetypes);
    } else if (filetypes.includes(entry.name.split('.').pop().toLowerCase())) {
      fsp.copyFile(srcPath, destPath);
    }
  }
}

function getStartCommand(platform) {
  if ('darwin' === platform) {
    return 'open';
  }

  if ('win32' === platform) {
    return 'start';
  }

  return 'xdg-open';
}

const esbuild = require('esbuild');
const fs = require('fs');
const fsp = require('fs/promises');
const path = require('path');

// mark webp files as external
const onResolvePlugin = {
  name: 'webp-external',
  setup(build) {
    build.onResolve({ filter: /.*\.webp$/ }, args => {
      return { path: args.path, external: true }
    })
  }
};

const buildOptions = {
  entryPoints: [
    './src/script/eom-calc.ts',
    './src/css/eom-calc.css'
  ],
  outdir: './dist/assets/',
  sourcemap: false,
  minify: true,
  bundle: true,
  legalComments: 'none',
  plugins: [onResolvePlugin],
};

// clean dist directory
// create .nojekyll file (for gh-pages)
// copy index.html and images to dist
fs.rmdirSync('./dist', {recursive: true});
fs.mkdirSync('./dist');
fs.closeSync(fs.openSync('./dist/.nojekyll', 'w'));
fsp.copyFile('./src/html/index.html', './dist/index.html');
fsp.copyFile('./src/html/accessory_prefixes.html', './dist/accessory_prefixes.html');
copyDir('./src/img', './dist/assets/img', ['webp', 'ico']);

// run serve or build
if (process.argv.includes('--serve')) {
  buildOptions.sourcemap = true;

  esbuild.serve({servedir: 'dist'}, buildOptions)
    .then(server => {
      console.log('Listening on ' + server.host + ':' + server.port);
      require('child_process').exec(getStartCommand(process.platform) + ' http://' + server.host + ':' + server.port);
    });
} else {
  esbuild.build(buildOptions);
}

// recursively copy directory, only copying files with given extension(s)
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

// returns platform specific command to start default browser
function getStartCommand(platform) {
  if ('darwin' === platform) {
    return 'open';
  }

  if ('win32' === platform) {
    return 'start';
  }

  return 'xdg-open';
}

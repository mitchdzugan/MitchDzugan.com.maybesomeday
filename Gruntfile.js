var p = require('child_process')
var fs = require('fs')

var clientNix = null;
var serverNix = null;

function reloadClient() {
  if (clientNix != null ) {
    clientNix.kill();
  }
  clientNix = p.spawn('nix-shell', {
      cwd: "client",
      stdio: [
        'pipe',
        0,
        0
      ]
  });
  clientNix.stdin.write("make config\n")
}

function buildClient() {
  if (clientNix != null) {
    clientNix.stdin.write("make build\n")
  } else {
    reloadClient()
  }
}

module.exports = function(grunt) {
  // Advanced config. Run specific tasks when specific files are added, changed or deleted.
  grunt.initConfig({
    watch: {
      shared: {
        files: ['shared/**/*.hs', 'shared/**/*.cabal', 'shared/**/*.nix'],
        tasks: ['shared'],
      },
      clientHaskell: {
        files: ['client/**/*.hs'],
        tasks: ['client-haskell'],
      },
      clientEnv: {
        files: ['client/**/*.cabal', 'client/**/*.nix'],
        tasks: ['client-env'],
      },
      serverHaskell: {
        files: ['server/**/*.hs'],
        tasks: ['server-haskell'],
      },
      serverEnv: {
        files: ['server/**/*.cabal', 'server/**/*.nix'],
        tasks: ['server-env'],
      },
    },
  });

  // Load the plugin that provides the "uglify" task.
  grunt.loadNpmTasks('grunt-contrib-watch');

  grunt.registerTask('shared', 'shared-changed', function() {
    grunt.log.write('shared changed...').ok();
    reloadClient();
  });

  grunt.registerTask('client-haskell', 'client-haskell-changed', function() {
    grunt.log.write('client-haskell changed...').ok();
    buildClient();
  });

  grunt.registerTask('client-env', 'client-env-changed', function() {
    grunt.log.write('client-env changed...').ok();
    reloadClient();
  });

  grunt.registerTask('server-haskell', 'server-haskell-changed', function() {
    grunt.log.write('server-haskell changed...').ok();
  });

  grunt.registerTask('server-env', 'server-haskell-env', function() {
    grunt.log.write('server-env changed...').ok();
  });
};

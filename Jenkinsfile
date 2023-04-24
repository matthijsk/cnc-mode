#!/usr/bin/env groovy

pipeline {
  agent {
    label 'built-in'
  }

  stages {
    stage('Test') {
      steps {
        sh label: "Run tests",
        script: "emacs -batch -l ert -l cnc-mode.el -l cnc-mode-test.el -f ert-run-tests-batch-and-exit"
      }
    }
  }
}

#!/usr/bin/env node
'use strict';

// this script is used for building the language server to be published

function abort(e) {
    console.error(e);
    process.exit(1);
}

process.on('uncaughtException', abort);

import * as fs from 'fs';
import { spawn } from 'child_process';
const fs_extra = require('fs-extra') as any;

const CRATE_PATH = '../kailua_langsvr';
const BINARY_NAME = 'kailua-langsvr' + (process.platform === 'win32' ? '.exe' : '');
const BINARY_PATH = '../target/release';
const PUBLISHED_BINARY_PATH = 'bin';

const cp = spawn('cargo', ['build', '--release'], {cwd: CRATE_PATH});
cp.on('error', abort);
cp.on('exit', (code, signal) => {
    if (code !== 0) {
        abort(`failed to compile the language server (code ${code})`);
    }

    fs.mkdir(PUBLISHED_BINARY_PATH, e => {
        if (e && e.code !== 'EEXIST') return;

        fs_extra.copy(BINARY_PATH + '/' + BINARY_NAME, PUBLISHED_BINARY_PATH + '/' + BINARY_NAME, e => {
            if (e) abort(e);
        });
    });
});

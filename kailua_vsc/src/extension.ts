'use strict';

import * as _ from 'lodash';
import * as path from 'path';
import * as net from 'net';
import { spawn, ChildProcess } from 'child_process';

import * as vscode from 'vscode';
import {
    LanguageClient, LanguageClientOptions, SettingMonitor, StreamInfo
} from 'vscode-languageclient';

declare var v8debug: any;

export function activate(context: vscode.ExtensionContext) {
    const debugPath = context.asAbsolutePath(path.join('..', 'target', 'debug', 'kailua-langsvr'));
    const releasePath = context.asAbsolutePath(path.join('..', 'target', 'release', 'kailua-langsvr'));

    const serverName = 'Kailua';

    // roughly follows the logic in vscode-languageserver client:
    // v8 is launched in the debugging mode, or vs code has received a debugging option
    let debugging = typeof v8debug === 'object';
    if (!debugging) {
        let args: string[] = (process as any).execArgv;
        debugging = args && args.some((arg) => /^--(?:debug|debug-brk)=?/.test(arg));
    }

    let languageClient: LanguageClient = null;

    const serverOptions = () => new Promise<ChildProcess | StreamInfo>((resolve, reject) => {
        function spawnWithArgs(...args: string[]): ChildProcess {
            let executablePath = debugging ? debugPath : releasePath;
            const cp = spawn(executablePath, args);
            cp.stderr.on('data', data => languageClient.outputChannel.append(_.isString(data) ? data : data.toString('utf-8')));
            return cp;
        }

        if (process.platform === 'win32') {
            // Windows seems to buffer the console output in the unit of multiple lines, so unusable for language server protocol
            const server = net.createServer(socket => {
                server.close();
                resolve({ reader: socket, writer: socket });
            });
            server.listen(0 /*random port*/, '127.0.0.1', () => {
                const cp = spawnWithArgs(`--packets-via-tcp=127.0.0.1:${server.address().port}`);
                // if we are using TCP the stdout is no longer used and any output should be logged as well
                cp.stdout.on('data', data => languageClient.outputChannel.append(_.isString(data) ? data : data.toString('utf-8')));
            });
        } else {
            resolve(spawnWithArgs('--packets-via-stdio'));
        }
    });

    let clientOptions: LanguageClientOptions = {
		documentSelector: ['lua'],
        synchronize: {
            fileEvents: [
                vscode.workspace.createFileSystemWatcher('**/*.lua'),
                vscode.workspace.createFileSystemWatcher('**/*.kailua'),
            ],
        },
        initializationOptions: {
            default_locale: vscode.env.language,
        },
	};
	
	languageClient = new LanguageClient('kailuaLanguageServer', serverName, serverOptions, clientOptions);

    const disposable = languageClient.start();
	context.subscriptions.push(disposable);
}

export function deactivate() {
}
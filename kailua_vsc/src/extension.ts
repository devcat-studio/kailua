'use strict';

import * as _ from 'lodash';
import * as fs from 'fs';
import * as path from 'path';
import * as net from 'net';
import { spawn, ChildProcess } from 'child_process';

import * as vscode from 'vscode';
import {
    LanguageClient, LanguageClientOptions, SettingMonitor, StreamInfo
} from 'vscode-languageclient';

declare var v8debug: any;

const INITIAL_CONFIG_FILE = `\
{\n\
    ${
        vscode.env.language === 'ko' ?
        '// 워크스페이스 디렉토리를 기준으로 체크를 시작할 파일명을 넣으세요.' :
        '// Put a relative path to the workspace directory to begin the checking.'
    }\n\
    "start_path": ""\n\
}\
`;

const SPAWN_FAILURE = (
    vscode.env.language === 'ko' ?
    '카일루아 언어 서버를 실행하는 데 실패했습니다' :
    'Failed to spawn the Kailua language server'
)

function isDebugging() {
    // roughly follows the logic in vscode-languageserver client:
    // v8 is launched in the debugging mode, or vs code has received a debugging option
    let debugging = typeof v8debug === 'object';
    if (!debugging) {
        let args: string[] = process.execArgv;
        debugging = args && args.some((arg) => /^--(?:debug|debug-brk)=?/.test(arg));
    }
    return debugging;
}

function openConfigurationFile() {
    const vscodePath = path.join(vscode.workspace.rootPath, '.vscode');
    fs.mkdir(vscodePath, e => {
        if (e && e.code !== 'EEXIST') return;

        const configPath = path.join(vscodePath, 'kailua.json');

        // first try the normal `file` scheme (for the existing file), then try to create a new file.
        const configUri = vscode.Uri.file(configPath);
        vscode.workspace.openTextDocument(configUri).then(doc => {
            vscode.window.showTextDocument(doc);
        }, _ => {
            const configUntitledUri = vscode.Uri.parse('untitled:' + configPath);
            vscode.workspace.openTextDocument(configUntitledUri).then(doc => {
                let edit = new vscode.WorkspaceEdit;
                edit.insert(doc.uri, new vscode.Position(0, 0), INITIAL_CONFIG_FILE);
                vscode.workspace.applyEdit(edit).then(_ => {
                    doc.save().then(() => {
                        vscode.workspace.openTextDocument(configUri).then(doc => {
                            vscode.window.showTextDocument(doc);
                        });
                    });
                });
            });
        });
    });
}

function initializeLanguageServer(context: vscode.ExtensionContext) {
    // in the debug mode: the debug executable at the Cargo workspace is exclusively used
    // in the release mode: try the executable at the Cargo workspace first, then the bundled executable
    const debugDevPath = context.asAbsolutePath(path.join('..', 'target', 'debug', 'kailua'));
    const releaseDevPath = context.asAbsolutePath(path.join('..', 'target', 'release', 'kailua'));
    const releasePath = context.asAbsolutePath(path.join('bin', 'kailua'));

    const extension = process.platform === 'win32' ? '.exe' : '';

    const serverName = 'Kailua';

    let languageClient: LanguageClient = null;

    const serverOptions = () => new Promise<ChildProcess | StreamInfo>((resolve, reject) => {
        function spawnWithArgs(...args: string[]): ChildProcess {
            let executablePath, env, mode;
            if (isDebugging()) {
                executablePath = debugDevPath + extension;
                env = {RUST_LOG: 'kailua_langsvr=debug', RUST_BACKTRACE: '1'};
                mode = 'debug mode w/ local impl';
            } else {
                try {
                    executablePath = releaseDevPath + extension;
                    mode = 'release mode w/ local impl';
                    fs.accessSync(executablePath, fs.constants.X_OK);
                } catch (_) {
                    try {
                        executablePath = releasePath + extension;
                        mode = 'release mode';
                        fs.accessSync(executablePath, fs.constants.X_OK);
                    } catch (_) {
                        // the fallback for platforms where `cargo install` is the only possible installation option.
                        // in those platforms `extension` will be "", that does not match any bundled executables
                        // so this branch gets executed.
                        executablePath = 'kailua' + extension;
                        mode = 'release mode w/ global installation';
                    }
                }
                env = {RUST_LOG: 'kailua_langsvr=info', RUST_BACKTRACE: '1'};
            }
            languageClient.outputChannel.append(`spawning a language server (${mode})\n`);

            const cp = spawn(executablePath, args, {env: env});
            cp.on('error', e => {
                languageClient.outputChannel.append(`failed to spawn a language server: ${e}\n`);
                vscode.window.showErrorMessage(`${SPAWN_FAILURE}: ${e}`);
            });
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
                const cp = spawnWithArgs('langsvr', `--tcp=127.0.0.1:${server.address().port}`);
                // if we are using TCP the stdout is no longer used and any output should be logged as well
                cp.stdout.on('data', data => languageClient.outputChannel.append(_.isString(data) ? data : data.toString('utf-8')));
            });
        } else {
            resolve(spawnWithArgs('langsvr', '--stdio'));
        }
    });

    let clientOptions: LanguageClientOptions = {
		documentSelector: ['lua'],
        synchronize: {
            fileEvents: [
                vscode.workspace.createFileSystemWatcher('**/*.lua'),
                vscode.workspace.createFileSystemWatcher('**/*.kailua'),
                vscode.workspace.createFileSystemWatcher('**/kailua.json'),
            ],
        },
        initializationOptions: {
            default_locale: vscode.env.language,
        },
	};
	
	languageClient = new LanguageClient('kailuaLanguageServer', serverName, serverOptions, clientOptions);
    return languageClient.start();
}

export function activate(context: vscode.ExtensionContext) {
    context.subscriptions.push(vscode.commands.registerCommand('Kailua.EditConfiguration', openConfigurationFile));
    context.subscriptions.push(initializeLanguageServer(context));
}

export function deactivate() {
}
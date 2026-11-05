// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as fs from 'fs';
import * as path from 'path';
import * as vscode from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions
} from 'vscode-languageclient/node';

const SERVER_NAME = 'dt-tools';
const SERVER_ARGS = ['lsp'];
const BUNDLED_SERVER = path.join('bin', SERVER_NAME);

let client: LanguageClient | undefined;

function serverCommand(context: vscode.ExtensionContext): string {
	const config = vscode.workspace.getConfiguration('dt-tools');
	const configured = config.get<string>('path');
	if (configured) {
		return configured;
	}

	const bundled = context.asAbsolutePath(BUNDLED_SERVER);
	if (process.platform === 'linux' && fs.existsSync(bundled)) {
		// VSIX does not always preserve the executable bit.
		// https://github.com/microsoft/vscode-arduino/commit/3649e62c52e46d9df3402340a6217eb3699ca451
		try {
			fs.accessSync(bundled, fs.constants.X_OK);
		} catch {
			fs.chmodSync(bundled, 0o755);
		}
		return bundled;
	}

	return SERVER_NAME;
}

function createClient(context: vscode.ExtensionContext): LanguageClient {
	console.log('createClient');
	const config = vscode.workspace.getConfiguration('dt-tools');
	const command = serverCommand(context);
	const extraEnv = config.get('extraEnv');
	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: {
			command,
			args: SERVER_ARGS,
			options: {
				env: extraEnv
			}
		},
		debug: {
			command,
			args: SERVER_ARGS,
			options: {
				env: extraEnv
			}
		}
	};

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'dts' }],
		markdown: {
			supportHtml: true
		},
	};

	// Create the language client and start the client.
	return new LanguageClient(
		'dt-tools',
		'dt-tools language server',
		serverOptions,
		clientOptions
	);
}

// Activates when a DTS file is opened
export async function activate(context: vscode.ExtensionContext) {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "dt-tools" is now active!');

	let disposable1 = vscode.workspace.onDidChangeConfiguration(async change => {
		console.log('Config changed');
		if (client === undefined) return;
		if (!change.affectsConfiguration("dt-tools")) return;
		if (client.isRunning()) {
			await client.stop();
			console.log('stopped');
		}

		client = createClient(context);
		await client.start();
	});
	context.subscriptions.push(disposable1);

	// The command has been defined in the package.json file
	// Now provide the implementation of the command with registerCommand
	// The commandId parameter must match the command field in package.json
	let disposable2 = vscode.commands.registerCommand('dt-tools.restart', async () => {
		if (client === undefined) return;
		vscode.window.showInformationMessage('Restarting language server!');
		await client.restart();
		vscode.window.showInformationMessage('Restarted language server!');
	});
	context.subscriptions.push(disposable2);

	client = createClient(context);

	// Start the client. This will also launch the server
	await client.start();
}

// This method is called when your extension is deactivated
export async function deactivate() {
	if (client === undefined) return;
	if (client.isRunning()) await client.stop();
}

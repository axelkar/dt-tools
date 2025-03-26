// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

function createClient(): LanguageClient {
	console.log('createClient');
	const serverConfig = vscode.workspace.getConfiguration('dt-tools-lsp.server');
	const path = serverConfig.get('path') as string;
	const extraEnv = serverConfig.get('extraEnv');
	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: {
			command: path,
			options: {
				env: extraEnv
			}
		},
		debug: {
			command: path,
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
		'dtToolsLsp',
		'Devicetree LSP server',
		serverOptions,
		clientOptions
	);
}

// Activates when a DTS file is opened
export async function activate(context: vscode.ExtensionContext) {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "dt-tools-lsp" is now active!');

	let disposable1 = vscode.workspace.onDidChangeConfiguration(async change => {
		console.log('Config changed');
		if (client === undefined) return;
		if (!change.affectsConfiguration("dt-tools-lsp.server")) return;
		if (client.isRunning()) {
			await client.stop();
			console.log('stopped');
		}

		client = createClient();
		await client.start();
	});
	context.subscriptions.push(disposable1);

	// The command has been defined in the package.json file
	// Now provide the implementation of the command with registerCommand
	// The commandId parameter must match the command field in package.json
	let disposable2 = vscode.commands.registerCommand('dt-tools-lsp.restart', async () => {
		if (client === undefined) return;
		vscode.window.showInformationMessage('Restarting LSP server!');
		await client.restart();
		vscode.window.showInformationMessage('Restarted LSP server!');
	});
	context.subscriptions.push(disposable2);

	client = createClient();

	// Start the client. This will also launch the server
	await client.start();
}

// This method is called when your extension is deactivated
export async function deactivate() {
	if (client === undefined) return;
	if (client.isRunning()) await client.stop();
}

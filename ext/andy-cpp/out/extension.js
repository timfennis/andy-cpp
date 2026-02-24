"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const vscode = __importStar(require("vscode"));
const child_process_1 = require("child_process");
const node_1 = require("vscode-languageclient/node");
let client;
class Lazy {
    initializer;
    _value;
    _isInitialized = false;
    constructor(initializer) {
        this.initializer = initializer;
    }
    get value() {
        if (!this._isInitialized) {
            this._value = this.initializer();
            this._isInitialized = true;
        }
        return this._value;
    }
}
function isNdcInstalled(ndcPath) {
    return new Promise((resolve) => {
        const cmd = process.platform === 'win32' ? `where "${ndcPath}"` : `which "${ndcPath}"`;
        (0, child_process_1.exec)(cmd, (error) => resolve(!error));
    });
}
async function activate(context) {
    const ndcPath = vscode.workspace.getConfiguration('andy-cpp').get('ndcPath', 'ndc');
    if (!await isNdcInstalled(ndcPath)) {
        const action = 'How to install';
        const choice = await vscode.window.showErrorMessage(`Andy C++: \`${ndcPath}\` was not found. Install ndc with \`cargo install --git https://github.com/timfennis/andy-cpp\`, or set \`andy-cpp.ndcPath\` in your VSCode settings.`, action);
        if (choice === action) {
            vscode.env.openExternal(vscode.Uri.parse('https://github.com/timfennis/andy-cpp'));
        }
        return;
    }
    const serverOptions = {
        run: { command: ndcPath, args: ["lsp"], transport: node_1.TransportKind.stdio },
        debug: { command: ndcPath, args: ["lsp"], transport: node_1.TransportKind.stdio }
    };
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'andy-cpp' }],
        synchronize: { fileEvents: vscode.workspace.createFileSystemWatcher('**/*.ndc') },
        outputChannel: vscode.window.createOutputChannel("Andy C++ LSP"),
        traceOutputChannel: vscode.window.createOutputChannel("Andy C++ LSP Trace"),
    };
    client = new node_1.LanguageClient('andy-cpp-lsp', 'Andy C++ LSP', serverOptions, clientOptions);
    context.subscriptions.push(client);
    client.start();
    const terminal = new Lazy(() => {
        return vscode.window.terminals.find((terminal) => terminal.name === "Andy C++") || vscode.window.createTerminal("Andy C++");
    });
    context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('andy-cpp', {
        createDebugAdapterDescriptor(session, executable) {
            const file = session.configuration.program;
            terminal.value.sendText(`${ndcPath} run "${file}"`);
            terminal.value.show();
            return executable;
        }
    }));
    context.subscriptions.push(vscode.commands.registerCommand('andy-cpp.runScript', () => {
        const editor = vscode.window.activeTextEditor;
        if (!editor) {
            vscode.window.showErrorMessage("No active editor. Open a file to run.");
            return;
        }
        const document = editor.document;
        const filePath = document.fileName;
        terminal.value.sendText(`${ndcPath} run ${filePath}`);
        terminal.value.show();
    }));
}
function deactivate() {
    return client?.stop();
}
//# sourceMappingURL=extension.js.map
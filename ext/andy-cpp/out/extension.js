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
const vscode = __importStar(require("vscode"));
// Adjust this to how your interpreter is invoked
const interpreterPath = "ndc";
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
function activate(context) {
    const terminal = new Lazy(() => {
        return vscode.window.terminals.find((terminal) => terminal.name === "Andy C++") || vscode.window.createTerminal("Andy C++");
    });
    context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('andy-cpp', {
        createDebugAdapterDescriptor(session, executable) {
            const file = session.configuration.program;
            terminal.value.sendText(`ndc run "${file}"`);
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
        terminal.value.sendText(`${interpreterPath} run ${filePath}`);
        terminal.value.show();
    }));
}
//# sourceMappingURL=extension.js.map
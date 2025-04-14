
import * as vscode from 'vscode';
import { exec } from 'child_process';
import * as path from 'path';
  
// Adjust this to how your interpreter is invoked
const interpreterPath = "ndc";

class Lazy<T> {
  private _value?: T;
  private _isInitialized = false;

  constructor(private readonly initializer: () => T) {}

  get value(): T {
    if (!this._isInitialized) {
      this._value = this.initializer();
      this._isInitialized = true;
    }
    return this._value!;
  }
}


export function activate(context: vscode.ExtensionContext) {


  const terminal = new Lazy(() => {
    return vscode.window.terminals.find((terminal) => terminal.name === "Andy C++") || vscode.window.createTerminal("Andy C++");
  });

  context.subscriptions.push(
    vscode.debug.registerDebugAdapterDescriptorFactory('andy-cpp', {
      createDebugAdapterDescriptor(session: vscode.DebugSession, executable: vscode.DebugAdapterExecutable | undefined) {
        const file = session.configuration.program;
        terminal.value.sendText(`ndc run "${file}"`);
        terminal.value.show();
        return executable; 
      }
    })
  );

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

"use strict";

module.exports = {
    config: {
        opacity: 1,
        updateChannel: 'canary',
        fontSize: 12,
        fontFamily: '"JetBrainsMono Nerd Font Mono", Menlo, "DejaVu Sans Mono", Consolas, "Lucida Console", monospace',
        fontWeight: 'normal',
        fontWeightBold: 'bold',
        lineHeight: 1,
        letterSpacing: 0,
        cursorColor: 'rgba(248,28,229,0.8)',
        cursorAccentColor: '#000',
        cursorShape: 'BLOCK',
        cursorBlink: false,
        foregroundColor: '#fff',
        backgroundColor: '#1e1e1e',
        selectionColor: 'rgba(248,28,229,0.3)',
        borderColor: '#333',
        css: '',
        termCSS: '',
        workingDirectory: 'C:\\Users\\zachf',
        showHamburgerMenu: true,
        showWindowControls: true,
        padding: '2px 4px',
        shell: 'C:\\Program Files\\PowerShell\\7\\pwsh.exe',
        shellArgs: ['--login'],
        env: {},
        bell: false,
        copyOnSelect: false,
        defaultSSHApp: true,
        quickEdit: false,
        macOptionSelectionMode: 'vertical',
        webGLRenderer: true,
        webLinksActivationKey: '',
        disableLigatures: false,
        disableAutoUpdates: false,
        screenReaderMode: false,
        preserveCWD: false,
        verminal: {
            fontFamily: '"JetBrainsMono Nerd Font Mono"',
            fontSize: 14,
            backgroundColor: '#1e1e1e',
            vibrancy: null
        }
    },
    plugins: ['hyper-search', 'hyper-pane', 'verminal', 'hyper-tabs-enhanced'],
    localPlugins: [],
    keymaps: {
        'window:devtools': 'ctrl+shift+q',
        'pane:next': 'ctrl+shift+l',
        'pane:prev': 'ctrl+shift+h'
    }
};

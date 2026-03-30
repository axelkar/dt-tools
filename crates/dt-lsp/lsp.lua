-- nvim --cmd 'luafile lsp.lua' a.dts
vim.lsp.config('dt-tools', {
  --cmd = {'../../target/debug/dt-tools-lsp'},
  cmd = {'cargo', 'run', '-p', 'dt-tools-lsp'},
  --cmd = vim.lsp.rpc.connect('127.0.0.1', 9257),

  filetypes = {
    'dts',

    -- For header (*.h) files
    'c',
    'cpp',
  },

  root_markers = { '.dt-tools.toml', '.git' },
})

vim.lsp.enable('dt-tools')

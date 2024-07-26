-- nvim -S lsp.lua a.dts
vim.lsp.start({
  name = 'DeviceTreeLSP',
  --cmd = {'../../target/debug/dt-lsp'},
  --cmd = {'cargo', 'run'},
  cmd = vim.lsp.rpc.connect('127.0.0.1', '9257'),
  -- root_dir = vim.fs.dirname(vim.fs.find({'setup.py', 'pyproject.toml'}, { upward = true })[1]),
})

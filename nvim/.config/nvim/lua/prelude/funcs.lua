-- TODO: maybe try to get that from kitty or zsh for faster/smoother load
-- also try Gitsign plugin ...!!! => `set statusline+=%{get(b:,'gitsigns_status','')}`
--
function _G.gitbranch()
	-- inside a git work tree?
	local inside = vim.fn.systemlist({ 'git', 'rev-parse', '--is-inside-work-tree' })
	if vim.v.shell_error ~= 0 or inside[1] ~= 'true' then
		return ''
	end

	-- short branch name; empty when detached
	local out = vim.fn.systemlist({ 'git', 'symbolic-ref', '--short', '-q', 'HEAD' })
	if vim.v.shell_error ~= 0 or not out[1] or out[1] == '' then
		return '' -- detached HEAD or unborn/other failure
	end

	return '[' .. out[1] .. ']'
end

config.bind("<f12>", 'inspector')
config.bind("<Ctrl-v>", 'spawn -d mpv --force-window=immediate {url}')
config.bind("<Ctrl-Shift-v>", 'hint links spawn -d mpv --force-window=immediate {url}')
c.content.proxy = 'none'
c.content.headers.user_agent = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.97 Safari/537.36'
c.downloads.location.directory = '~/downloads/'
c.downloads.location.prompt = False
c.editor.command = ['emacsclient', '-c', '"{}"']
c.scrolling.smooth = False
c.content.pdfjs = True
c.scrolling.bar = 'always'

ui_font = '12pt monospace' #I'm getting old and going blind
c.fonts.completion.category = ui_font
c.fonts.completion.entry = ui_font
c.fonts.debug_console = ui_font
c.fonts.downloads = ui_font
c.fonts.hints = ui_font
c.fonts.keyhint = ui_font
c.fonts.messages.error = ui_font
c.fonts.messages.warning = ui_font
c.fonts.statusbar = ui_font
c.fonts.tabs = ui_font
c.fonts.prompts = ui_font

config.bind('<Ctrl-e>', 'open-editor')
config.bind("<f11>", 'config-source') 

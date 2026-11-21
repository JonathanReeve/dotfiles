{ config, vars, ... }:

{
  programs.qutebrowser = {
    enable = true;
    extraConfig = ''
      c.statusbar.padding = {'top': 5, 'bottom': 5, 'left': 3, 'right': 3}
      c.tabs.padding = {'top': 2, 'bottom': 2, 'left': 2, 'right': 2}
    '';
    keyBindings = {
      normal = {
        "N" = "tab-next";
        "E" = "tab-prev";
        "K" = "search-prev";
        "l" = "mode-enter insert";
        "n" = "scroll down";
        "e" = "scroll up";
        "i" = "scroll right";
        "j" = "search-next";
        "b" = "set-cmd-text -s :tab-select ";
        "gL" = "open javascript:location.href='org-protocol://capture?template=l&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&body='+encodeURIComponent(document.getSelection())";
        "gM" = "open javascript:location.href='org-protocol://capture?template=m&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)";
        "gT" = "open javascript:location.href='org-protocol://capture?template=T&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)";
        "gR" = "open javascript:location.href='org-protocol://roam-ref?template=r&ref='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)";
        "gB" = "spawn -m ${vars.scripts}/downloadBook.py {url}";
        "gb" = "spawn --userscript libro-capture";
        "gV" = "spawn --userscript vulpea-bookmark";
        "gO" = "spawn --userscript org-link";
        "gW" = "spawn --userscript wikidata-movie";
        "pf" = "spawn --userscript qute-pass";
        "gz" = "jseval var d=document,s=d.createElement('script';;s.src='https://www.zotero.org/bookmarklet/loader.js';(d.body?d.body:d.documentElement;.appendChild(s;;void(0;;";
        "gr" = "open javascript:location.href='https://app.raindrop.io/add?link='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&note='+encodeURIComponent((document.getSelection&&document.getSelection().toString())||'')";
        "t" = "set-cmd-text -s :open -t";
        "Y" = "yank selection";
        "O" = "set-cmd-text :open {url:pretty}";
        "<Alt-Left>" = "back";
        "<Alt-Right>" = "forward";
      };
    };
    searchEngines = {
      "DEFAULT" = "https://duckduckgo.com/?q={}";
      "g" = "https://www.google.com/search?q={}";
      "l" = "https://www.google.com/search?hl=en&q={}&btnI=I";
      "w" = "https://en.wikipedia.org/w/index.php?search={}";
      "wd" = "https://www.wikidata.org/w/index.php?search={}";
      "gs" = "https://scholar.google.com/scholar?q={}";
      "b" = "https://www.google.com/search?tbm=bks&q={}";
      "aw" = "https://wiki.archlinux.org/?search={}";
      "o" = "https://search.nixos.org/options?channel=unstable&from=0&size=50&sort=relevance&query={}";
      "p" = "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&query={}";
      "d" = "https://en.wiktionary.org/wiki/{}";
      "v" = "https://eo.wiktionary.org/wiki/{}";
      "melpa" = "https://melpa.org/#/?q={}";
      "s" = "http://stackoverflow.com/search?q={}";
      "ss" = "https://www.semanticscholar.org/search?q={}";
      "m" = "https://maps.google.com/maps?q={}";
      "c" = "https://clio.columbia.edu/quicksearch?q={}";
      "gh" = "https://github.com/search?q={}&type=Repositories";
      "h" = "https://hackage.haskell.org/packages/search?terms={}";
      "ho" = "https://hoogle.haskell.org/?hoogle={}";
      "libgen" = "https://libgen.is/search.php?req={}";
      "a" = "https://annas-archive.gl/search?q={}";
      "viki" = "https://eo.wikipedia.org/w/index.php?search={}";
      "ia" = "https://archive.org/details/texts?and%5B%5D={}&sin=";
      "mm" = "https://muse-jhu-edu.ezproxy.cul.columbia.edu/search?action=search&query=content:{}:and&limit=journal_id:131&min=1&max=10&t=search_journal_header";
    };
    settings = {
      content.local_content_can_access_remote_urls = true;
      content.headers.accept_language = "eo,en-US,en,fr";
      fonts = {
        completion.category = "11pt '${vars.font}'";
        default_family = vars.font;
        default_size = "11pt";
        tabs.selected = "11pt '${vars.font}'";
        tabs.unselected = "11pt '${vars.font}'";
      };
      hints.chars = "arstdhneio";
      url.default_page = "${vars.scripts}/homepage/homepage.html";
      colors.webpage.darkmode.enabled = true;
      qt.args = [ "disable-accelerated-video-decode" ];
    };
  };

  xdg.dataFile."qutebrowser/userscripts" = {
    source = config.lib.file.mkOutOfStoreSymlink "${vars.scripts}/qutebrowser-userscripts";
  };
}

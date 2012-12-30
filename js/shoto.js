
function makeLanguageChanger() {
  function englishPage() { return $('html').attr('lang') === 'en'}
  function finnishPage() { return $('html').attr('lang') === 'fi'}
  function rootPage() {return window.location.pathname == "/"}

  function parseEnglishUrl() {
    if(rootPage()) {
      return "index_en.html"
    } else {
      var pathname = window.location.pathname
      return pathname.slice(0, -5) + "_en.html"
    }
  }

  function parseFinnishUrl() {
    var pathname = window.location.pathname
    return pathname.slice(0, -8) + ".html"
  }

  function navigateToFinnishPage() {
    if(englishPage()) {
      window.location.pathname = parseFinnishUrl()
    }
  }

  function navigateToEnglishPage() {
    if(finnishPage()) {
      window.location.pathname = parseEnglishUrl()
    }
  }

  return {
    navigateToFinnishPage: navigateToFinnishPage,
    navigateToEnglishPage: navigateToEnglishPage
  }
}


$(window).load(function() {
  $('.flexslider').flexslider()
  var languageChanger = makeLanguageChanger()

  $('#finnish').click(languageChanger.navigateToFinnishPage)
  $('#english').click(languageChanger.navigateToEnglishPage)

})

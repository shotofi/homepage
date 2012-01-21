function parseNavigation() {
  var galleryPage = /^kuv[ia][ta].*html$/
  var location = window.location.pathname.slice(1)
  if(galleryPage.test(location)) {
    return localizePage("muistoja")
  } else {
    return location
  }
}

function naviLinkToText(href) {
  var link = $('.tabs a[href="' + href + '"]')
  if(link.length > 1) return
  var linkText = link.text()
  link.parent().text(linkText)
}

function englishPage() { return $('html').attr('lang') === 'en'}

function localizePage(page) {
  if(englishPage()) {
    return page + "_en.html"
  } else {
    return page + ".html"
  }
}

function parseEnglishUrl() {
  var pathname = window.location.pathname
  return pathname.slice(0, -5) + "_en.html"
}

function parseFinnishUrl() {
  var pathname = window.location.pathname
  return pathname.slice(0, -8) + ".html"
}

function changeLanguage() {
  if(englishPage()) {
    window.location.pathname = parseFinnishUrl()
  } else {
    window.location.pathname = parseEnglishUrl()
  }
}

$(function() {
  naviLinkToText(parseNavigation())
})


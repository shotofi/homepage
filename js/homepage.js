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

function localizePage(page) {
  if($('html').attr('lang') === 'en') {
    return page + "_en.html"
  } else {
    return page + ".html"
  }
}


$(function() {
  naviLinkToText(parseNavigation())
})


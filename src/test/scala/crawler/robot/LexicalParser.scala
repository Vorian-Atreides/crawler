package crawler.robot

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.should._

class LexicalParserSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val supportedLines = Table(
    ("data"),
    ("User-Agent: *"),
    ("User-Agent: google-bot-news"),
    ("User-Agent: google"),
    ("User-Agent: google # with a comment"),
    ("Allow: /"),
    ("Disallow: /private"),
    ("Allow: /images # with a comment"),
  )

  property("supported token should be parsed without error") {
    forAll(supportedLines) { (data: String) =>
      val result = LexicalParser.parse(data)
      result.errors.length should be (0)
      result.tokens.length should be (1)
    }
  }

  val ignoredLines = Table(
    ("data"),
    (""),
    ("# Just a comment about something important."),
    ("\n"),
    ("      "),
  )

  property("comments and empty lines should not produce a token nor an error") {
    forAll(ignoredLines) { (data: String) =>
      val result = LexicalParser.parse(data)
      result.errors.length should be (0)
      result.tokens.length should be (0)
    }
  }

  val invalidLines = Table(
    ("data"),
    ("Sitemap: https://something.com/sitemap.xml"),
    ("Unsupported: key and value"),
    ("not a key value format"),
  )

  property("unsupported lines should return an error and no token") {
    forAll(invalidLines) { (data: String) =>
      val result = LexicalParser.parse(data)
      result.errors.length should be (1)
      result.tokens.length should be (0)
    }
  }

  val validMultilines = Table(
    ("data"),
    (
      """
        |User-agent: *
        |Disallow: /search
        |Allow: /search/about
        |Allow: /search/static
        |Allow: /search/howsearchworks
        |Disallow: /sdch
        |Disallow: /groups
        |Disallow: /index.html?
        |Disallow: /?
        |Allow: /?hl=
        |Disallow: /?hl=*&
        |Allow: /?hl=*&gws_rd=ssl$
        |Disallow: /?hl=*&*&gws_rd=ssl
        |Allow: /?gws_rd=ssl$
        |Allow: /?pt1=true$
        |Disallow: /imgres
        |Disallow: /u/
        |Disallow: /preferences
        |Disallow: /setprefs
        |Disallow: /default
        |Disallow: /m?
        |Disallow: /m/
        |Allow:    /m/finance
        |Disallow: /wml?
        |Disallow: /wml/?
        |Disallow: /wml/search?
        |Disallow: /xhtml?
        |Disallow: /xhtml/?
        |Disallow: /xhtml/search?
        |Disallow: /xml?
        |Disallow: /imode?
        |Disallow: /imode/?
        |Disallow: /imode/search?
        |Disallow: /jsky?
        |Disallow: /jsky/?
        |Disallow: /jsky/search?
        |Disallow: /pda?
        |Disallow: /pda/?
        |Disallow: /pda/search?
        |Disallow: /sprint_xhtml
        |Disallow: /sprint_wml
        |Disallow: /pqa
        |Disallow: /palm
        |Disallow: /gwt/
        |Disallow: /purchases
        |Disallow: /local?
        |Disallow: /local_url
        |Disallow: /shihui?
        |Disallow: /shihui/
        |Disallow: /products?
        |Disallow: /product_
        |Disallow: /products_
        |Disallow: /products;
        |Disallow: /print
        |Disallow: /books/
        |Disallow: /bkshp?*q=*
        |Disallow: /books?*q=*
        |Disallow: /books?*output=*
        |Disallow: /books?*pg=*
        |Disallow: /books?*jtp=*
        |Disallow: /books?*jscmd=*
        |Disallow: /books?*buy=*
        |Disallow: /books?*zoom=*
        |Allow: /books?*q=related:*
        |Allow: /books?*q=editions:*
        |Allow: /books?*q=subject:*
        |Allow: /books/about
        |Allow: /booksrightsholders
        |Allow: /books?*zoom=1*
        |Allow: /books?*zoom=5*
        |Allow: /books/content?*zoom=1*
        |Allow: /books/content?*zoom=5*
        |Disallow: /ebooks/
        |Disallow: /ebooks?*q=*
        |Disallow: /ebooks?*output=*
        |Disallow: /ebooks?*pg=*
        |Disallow: /ebooks?*jscmd=*
        |Disallow: /ebooks?*buy=*
        |Disallow: /ebooks?*zoom=*
        |Allow: /ebooks?*q=related:*
        |Allow: /ebooks?*q=editions:*
        |Allow: /ebooks?*q=subject:*
        |Allow: /ebooks?*zoom=1*
        |Allow: /ebooks?*zoom=5*
        |Disallow: /patents?
        |Disallow: /patents/download/
        |Disallow: /patents/pdf/
        |Disallow: /patents/related/
        |Disallow: /scholar
        |Disallow: /citations?
        |Allow: /citations?user=
        |Disallow: /citations?*cstart=
        |Allow: /citations?view_op=new_profile
        |Allow: /citations?view_op=top_venues
        |Allow: /scholar_share
        |Disallow: /s?
        |Allow: /maps?*output=classic*
        |Allow: /maps?*file=
        |Allow: /maps/d/
        |Disallow: /maps?
        |Disallow: /mapstt?
        |Disallow: /mapslt?
        |Disallow: /maps/stk/
        |Disallow: /maps/br?
        |Disallow: /mapabcpoi?
        |Disallow: /maphp?
        |Disallow: /mapprint?
        |Disallow: /maps/api/js/
        |Allow: /maps/api/js
        |Disallow: /maps/api/place/js/
        |Disallow: /maps/api/staticmap
        |Disallow: /maps/api/streetview
        |Disallow: /maps/_/sw/manifest.json
        |Disallow: /mld?
        |Disallow: /staticmap?
        |Disallow: /maps/preview
        |Disallow: /maps/place
        |Disallow: /maps/timeline/
        |Disallow: /help/maps/streetview/partners/welcome/
        |Disallow: /help/maps/indoormaps/partners/
        |Disallow: /lochp?
        |Disallow: /center
        |Disallow: /ie?
        |Disallow: /blogsearch/
        |Disallow: /blogsearch_feeds
        |Disallow: /advanced_blog_search
        |Disallow: /uds/
        |Disallow: /chart?
        |Disallow: /transit?
        |Allow:    /calendar$
        |Allow:    /calendar/about/
        |Disallow: /calendar/
        |Disallow: /cl2/feeds/
        |Disallow: /cl2/ical/
        |Disallow: /coop/directory
        |Disallow: /coop/manage
        |Disallow: /trends?
        |Disallow: /trends/music?
        |Disallow: /trends/hottrends?
        |Disallow: /trends/viz?
        |Disallow: /trends/embed.js?
        |Disallow: /trends/fetchComponent?
        |Disallow: /trends/beta
        |Disallow: /trends/topics
        |Disallow: /musica
        |Disallow: /musicad
        |Disallow: /musicas
        |Disallow: /musicl
        |Disallow: /musics
        |Disallow: /musicsearch
        |Disallow: /musicsp
        |Disallow: /musiclp
        |Disallow: /urchin_test/
        |Disallow: /movies?
        |Disallow: /wapsearch?
        |Allow: /safebrowsing/diagnostic
        |Allow: /safebrowsing/report_badware/
        |Allow: /safebrowsing/report_error/
        |Allow: /safebrowsing/report_phish/
        |Disallow: /reviews/search?
        |Disallow: /orkut/albums
        |Disallow: /cbk
        |Disallow: /recharge/dashboard/car
        |Disallow: /recharge/dashboard/static/
        |Disallow: /profiles/me
        |Allow: /profiles
        |Disallow: /s2/profiles/me
        |Allow: /s2/profiles
        |Allow: /s2/oz
        |Allow: /s2/photos
        |Allow: /s2/search/social
        |Allow: /s2/static
        |Disallow: /s2
        |Disallow: /transconsole/portal/
        |Disallow: /gcc/
        |Disallow: /aclk
        |Disallow: /cse?
        |Disallow: /cse/home
        |Disallow: /cse/panel
        |Disallow: /cse/manage
        |Disallow: /tbproxy/
        |Disallow: /imesync/
        |Disallow: /shenghuo/search?
        |Disallow: /support/forum/search?
        |Disallow: /reviews/polls/
        |Disallow: /hosted/images/
        |Disallow: /ppob/?
        |Disallow: /ppob?
        |Disallow: /accounts/ClientLogin
        |Disallow: /accounts/ClientAuth
        |Disallow: /accounts/o8
        |Allow: /accounts/o8/id
        |Disallow: /topicsearch?q=
        |Disallow: /xfx7/
        |Disallow: /squared/api
        |Disallow: /squared/search
        |Disallow: /squared/table
        |Disallow: /qnasearch?
        |Disallow: /app/updates
        |Disallow: /sidewiki/entry/
        |Disallow: /quality_form?
        |Disallow: /labs/popgadget/search
        |Disallow: /buzz/post
        |Disallow: /compressiontest/
        |Disallow: /analytics/feeds/
        |Disallow: /analytics/partners/comments/
        |Disallow: /analytics/portal/
        |Disallow: /analytics/uploads/
        |Allow: /alerts/manage
        |Allow: /alerts/remove
        |Disallow: /alerts/
        |Allow: /alerts/$
        |Disallow: /ads/search?
        |Disallow: /ads/plan/action_plan?
        |Disallow: /ads/plan/api/
        |Disallow: /ads/hotels/partners
        |Disallow: /phone/compare/?
        |Disallow: /travel/clk
        |Disallow: /travel/hotelier/terms/
        |Disallow: /hotelfinder/rpc
        |Disallow: /hotels/rpc
        |Disallow: /commercesearch/services/
        |Disallow: /evaluation/
        |Disallow: /chrome/browser/mobile/tour
        |Disallow: /compare/*/apply*
        |Disallow: /forms/perks/
        |Disallow: /shopping/suppliers/search
        |Disallow: /ct/
        |Disallow: /edu/cs4hs/
        |Disallow: /trustedstores/s/
        |Disallow: /trustedstores/tm2
        |Disallow: /trustedstores/verify
        |Disallow: /adwords/proposal
        |Disallow: /shopping/product/
        |Disallow: /shopping/seller
        |Disallow: /shopping/ratings/account/metrics
        |Disallow: /shopping/reviewer
        |Disallow: /about/careers/applications/
        |Disallow: /landing/signout.html
        |Disallow: /webmasters/sitemaps/ping?
        |Disallow: /ping?
        |Disallow: /gallery/
        |Disallow: /landing/now/ontap/
        |Allow: /searchhistory/
        |Allow: /maps/reserve
        |Allow: /maps/reserve/partners
        |Disallow: /maps/reserve/api/
        |Disallow: /maps/reserve/search
        |Disallow: /maps/reserve/bookings
        |Disallow: /maps/reserve/settings
        |Disallow: /maps/reserve/manage
        |Disallow: /maps/reserve/payment
        |Disallow: /maps/reserve/receipt
        |Disallow: /maps/reserve/sellersignup
        |Disallow: /maps/reserve/payments
        |Disallow: /maps/reserve/feedback
        |Disallow: /maps/reserve/terms
        |Disallow: /maps/reserve/m/
        |Disallow: /maps/reserve/b/
        |Disallow: /maps/reserve/partner-dashboard
        |Disallow: /about/views/
        |Disallow: /intl/*/about/views/
        |Disallow: /local/dining/
        |Disallow: /local/place/products/
        |Disallow: /local/place/reviews/
        |Disallow: /local/place/rap/
        |Disallow: /local/tab/
        |Allow: /finance
        |Allow: /js/
        |
        |# AdsBot
        |User-agent: AdsBot-Google
        |Disallow: /maps/api/js/
        |Allow: /maps/api/js
        |Disallow: /maps/api/place/js/
        |Disallow: /maps/api/staticmap
        |Disallow: /maps/api/streetview
        |
        |# Certain social media sites are whitelisted to allow crawlers to access page markup when links to google.com/imgres* are shared. To learn more, please contact images-robots-whitelist@google.com.
        |User-agent: Twitterbot
        |Allow: /imgres
        |
        |User-agent: facebookexternalhit
        |Allow: /imgres
        |
        |Sitemap: https://www.google.com/sitemap.xml
        |""".stripMargin),
    (
      """
        |User-agent: *
        |
        |# Miscellaneous
        |Disallow: /mijn/
        |Disallow: /*/print/*
        |Disallow: /koop/zoeksuggestie/
        |Disallow: /huur/zoeksuggestie/
        |Disallow: /nieuwbouw/zoeksuggestie/
        |Disallow: /recreatie/zoeksuggestie/
        |Disallow: /europe/zoeksuggestie/
        |Disallow: /*/brochure/download/
        |Disallow: */uitgebreid-zoeken/*
        |Disallow: /makelaars/*/woningaanbod/*
        |Disallow: /zoekwidget/*
        |Allow: /zoekwidget/$
        |Disallow: /relatedobjects
        |Disallow: /mijn/huis/wonen/toevoegen/
        |Disallow: /*/woningrapport/
        |
        |# Prevent bots from indexing combinations of locations
        |Disallow: /koop/*,*
        |Disallow: /huur/*,*
        |Disallow: /nieuwbouw/*,*
        |Disallow: /recreatie/*,*
        |Disallow: /europe/*,*
        |
        |
        |
        |Sitemap: https://www.funda.nl/sitemap_index.xml
        |""".stripMargin)
  )

  property("valid multi-lines robots.txt") {
    forAll(validMultilines) { (data: String) =>
      val result = LexicalParser.parse(data)
      // Calculate the number of supported directive minus the unsupported Sitemap.
      val expected = data.split("([Aa]llow:|[Dd]isallow:|[Uu]ser-[Aa]gent:)").length - 1
      result.errors.length should be (1)
      result.tokens.length should be (expected)
    }
  }
}
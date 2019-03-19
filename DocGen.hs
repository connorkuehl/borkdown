module DocGen where

import Parse

generateHtml     :: Document -> String
generateHtml doc = "<html><body>" ++ (concat . map generateParagraph) doc ++ "</body></html>"

generateParagraph                         :: Paragraph -> String
generateParagraph (Heading level content) = openHeading ++ content ++ closeHeading
    where openHeading = "<h" ++ show level ++ ">"
          closeHeading = "</h" ++ show level ++ ">"

generateParagraph (Para inlines)    = "<p>" ++ content inlines ++ "</p>"
generateParagraph (OrdList items)   = "<ol>" ++ generateListItems items ++ "</ol>"
generateParagraph (UnordList items) = "<ul>" ++ generateListItems items ++ "</ul>"

generateListItems    :: [Item] -> String
generateListItems is = (concat . map generateListItem) is

generateListItem                :: Item -> String
generateListItem (Ordered is)   = listItem is
generateListItem (Unordered is) = listItem is

listItem   :: [Inline] -> String
listItem i = "<li>" ++ content i ++ "</li>"

generateInline                 :: Inline -> String
generateInline (Plain s)       = s
generateInline (Link desc url) = "<a href=" ++ url ++ ">" ++ desc ++ "</a>"
generateInline (Italic i)      = "<em>" ++ generateInline i ++ "</em>"
generateInline (Bold b)        = "<strong>" ++ generateInline b ++ "</strong>"
generateInline (Code c)        = "<code>" ++ c ++ "</code>"

content         :: [Inline] -> String
content inlines = (concat . map generateInline) inlines

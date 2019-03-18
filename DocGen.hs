module DocGen where

import Parse

generateHtml     :: Document -> String
generateHtml doc = "<html><body>" ++ (concat . map generateParagraph) doc ++ "</body></html>"
  where generateParagraph :: Paragraph -> String
        generateParagraph (Heading level content) = openHeading ++ content ++ closeHeading
            where openHeading = "<h" ++ show level ++ ">"
                  closeHeading = "</h" ++ show level ++ ">"
        generateParagraph (Para inlines) = "<p>" ++ content ++ "</p>"
            where content = (concat . map generateInline) inlines
        generateInline (Plain s) = s
        generateInline (Link desc url) = "<a href=" ++ url ++ ">" ++ desc ++ "</a>"
        generateInline (Italic i) = "<em>" ++ generateInline i ++ "</em>"
        generateInline (Bold b) = "<strong>" ++ generateInline b ++ "</strong>"
        generateInline (Code c) = "<pre>" ++ c ++ "</pre>"

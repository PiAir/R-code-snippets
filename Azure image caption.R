library(tools)
library(httr)
vision_api_endpoint <- "https://northeurope.api.cognitive.microsoft.com/vision/v1.0"
vision_api_key <- "5536b1e85364ga5a4299450d3dtbaf2" # add your own, this one is fake!

random_image <- function() {
  ## Return the URL random image in Wikimedia Commons
  random_query <- paste0("https://commons.wikimedia.org/w/api.php?",
                         "action=query",
                         "&generator=random", # get a random page
                         "&grnlimit=1",       # return 1 page
                         "&grnnamespace=6",   # category: File
                         "&prop=imageinfo",
                         "&iiprop=url|size|extmetadata",
                         "&iiurlheight=1080",  # limit images height (sometimes)
                         "&format=json&formatversion=2")
  random_response <- POST(random_query)
  output <- content(random_response)
  url <- output$query$pages[[1]]$imageinfo[[1]]$url
  ext <- tolower(file_ext(url))
  w <- output$query$pages[[1]]$imageinfo[[1]]$width
  h <- output$query$pages[[1]]$imageinfo[[1]]$height
  size <- output$query$pages[[1]]$imageinfo[[1]]$size
  desc <- output$query$pages[[1]]$imageinfo[[1]]$extmetadata$ImageDescription$value 
  if(w<50 || h<50) stop("Image too small") 
  if(size > 4000000) stop("Image too large")
  if(!(ext %in% c("jpg","jpeg","png","gif","bmp"))) stop(paste("invalid image type:",ext))
  attr(url, "dims") <- c(w=w,h=h)
  attr(url, "desc") <- desc
  url
} 

image_caption <- function(URL) {
  reqURL = paste0(vision_api_endpoint,
                  "/analyze?visualFeatures=Description",
                  "&details=Celebrities,Landmarks")
  cat("reqURL = ", reqURL, "\n")
  APIresponse = POST(url = reqURL,
                     content_type('application/json'),
                     add_headers(.headers = c('Ocp-Apim-Subscription-Key' = vision_api_key)),
                     body=list(url = URL),
                     encode = "json") 
  
  df = content(APIresponse)
  
  str(df)
  
  
  cat(URL, "\n")
  cat(paste0("Confidence: ",df$description$captions[[1]]$confidence,"\n"))
  cat("Wikimedia Commons description:\n", attr(URL,"desc"),  "\n")
  cat("Vision API description:\n",  df$description$captions[[1]]$text,"\n")
  invisible(df)
}


u <- random_image()
image_caption(u)
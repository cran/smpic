% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main_functions.R
\name{smpic_view}
\alias{smpic_view}
\title{Preview a social media sized ggplot}
\usage{
smpic_view(p, sm = c("facebook_shared_images", "facebook_profile_image",
  "facebook_cover_photo", "facebook_shared_link", "facebook_highlighted_image",
  "facebook_event_image", "linkedin_profile_image", "linkedin_standard_logo",
  "linkedin_hero_image", "linkedin_business_banner_image",
  "linkedin_square_logo", "youtube_channel_cover_photo",
  "youtube_video_uploads", "instagram_profile_image",
  "instagram_photo_thumbnails", "instagram_photo_size", "twitter_header_photo",
  "twitter_profile_image", "twitter_in-stream_photo",     
  "pinterest_profile_image", "pinterest_board_display",
  "pinterest_board_display_thumbnails", "pinterest_pin_sizes",
  "tumblr_profile_image", "tumblr_image_posts", "g+_profile_image",
  "g+_cover_image", "g+_shared_image", "g+_shared_video",
  "g+_shared_image_square", "ello_banner_image", "ello_profile_image"),
  text_factor = 1, custom_dims = NULL)
}
\arguments{
\item{p}{the ggplot you want to preview.}

\item{sm}{the social media picture type you want it sized to.}

\item{text_factor}{a factor for the text in the plot. Change it if the text
looks to small.}

\item{custom_dims}{a vector of width and height for your own custom size.}
}
\value{
a plot
}
\description{
Preview a social media sized ggplot
}
\examples{
library(dplyr)
library(ggplot2)
library(smpic)

p <- ggplot(iris) +
     geom_point(aes(Petal.Length, Petal.Width, color = Species), show.legend = FALSE) +
       geom_label(data = summarise_if(group_by(iris, Species), is.numeric, mean),
                  aes(Petal.Length, Petal.Width, label = Species, color = Species),
                  show.legend = FALSE) +
       labs(x = "Petal Length", y = "Petal Width",
            title = "Look mom, a flower plot!",
            subtitle = "Yet another iris data set visualization.",
            caption = "Source: iris") +
       theme_minimal()

  smpic_view(p, sm = "facebook_shared_images", text_factor = 2.2)
}

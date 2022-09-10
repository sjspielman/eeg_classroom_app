# Define variables -----------------------------
library(magrittr)
expected_channels <-c("AF3", "AF4", "F3", "F4", "F7", "F8", "FC5", "FC6", "T7", "T8", "P7", "P8", "O1", "O2")
channel_location_df <- eegUtils:::electrodeLocs %>%
  dplyr::filter(electrode %in% expected_channels)

# alpha waves are in [8,12]
alpha_freq_min <- 8
alpha_freq_max <- 12


#' Function to read and optionally reference an EDF file
#'
#' @param filepath Path to EDI file, with a local default for dev
#' @param reference Logical (default `TRUE`) for whether to perform referencing on signals.
#'   If `TRUE`, data will be referenced by the mean.
#'
#' @return
#' @export
#'
#' @examples
read_edf_file <- function(filepath = "data/Charliemusic_2019.04.12_08.52.45.edf",
                          reference = TRUE) {

  raw_data <- eegUtils::import_raw(filepath)

  # Remove unused columns from `signals`
  remove_columns <- c("INTERPOLATED", "GYROX", "GYROY", "MARKER", "MARKER_HARDWARE", "SYNC", "COUNTER")
  raw_data$signals <- raw_data$signals %>%
    dplyr::select(-dplyr::all_of(remove_columns),
                  -dplyr::contains("CQ"),
                  # eegUtils parsed this into `raw_data$timings$time`
                  -dplyr::contains("TIME_STAMP"))

  # Channels should now match `channel_options`
  if (!identical( sort(expected_channels), sort(names(raw_data$signals)) ) ) {
    stop("ERROR: Unexpected or missing channels in the input EDF file.")
  }

  # Add in channel info
  raw_data$chan_info <- channel_location_df

  # Reference to the mean and return
  if (reference) {
    return(eegUtils::eeg_reference(raw_data))
  } else {
    return(raw_data)
  }

}






#' Function to perform PSD calculations
#'
#' @param edf_data eegUtils object
#'
#' @return df with three columns: frequency, electrode, power
prep_psd_data <- function(edf_data) {

  eegUtils::compute_psd(edf_data) %>%
    tidyr::pivot_longer(-frequency,
                        names_to = "channel",
                        values_to = "power")
}




#' Function to make a PSD plot using ggplot2
#'
#' @param plot_data Output df from prep_psd_data().
#' @param plot_channels vector of channels to include in plot. _This_ is why we use
#'   ggplot2 and not eegUtils::plot_psd()
#' @param frequency_range vector of c(start,stop) for frequencies, here the X-axis.
#'  Default is `NULL` which plots all frequencies. NOT IMPLEMENTED YET.
#'
#' @return ggplot2 plot object
make_psd_plot <- function(plot_data, plot_channels, frequency_range = NULL) {


  # Reference: https://github.com/craddm/eegUtils/blob/0a1ab99e1b9cb2befd0b30489f0aeaf83d13469d/R/frequency_plotting.R#L189-L204
  plot <- plot_data %>%
    dplyr::filter(channel %in% plot_channels) %>%
    dplyr::mutate(power = 10*log10(power)) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = frequency,
                 y = power,
                 color = channel) +
    ggplot2::stat_summary(geom = "line",
                          fun = mean) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(x = "Frequency (Hz)",
                  y = expression(paste(mu, V^2, "/ Hz(dB)"))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = ggplot2::rel(1.25)),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1.25)),
      legend.text = ggplot2::element_text(size = ggplot2::rel(1.25)),
      legend.title = ggplot2::element_text(size = ggplot2::rel(1.25))
    )

  if (!is.null(frequency_range)) {
    plot <- plot + xlim(frequency_range)
  }

  return(plot)
}



#' Function to make a topoplot of power values
#'
#' @param psd_data df from `make_psd_data()`
#' @param topo_bins number of bins for the topoplot. Higher = finer contour.
#'  Default: 6 (same as eegUtils)
#' @param freq_min minimum frequency to include in topoplot. Default: -Inf
#' @param freq_max maximum frequency to include in topoplot. Default: Inf
#'
#' @return ggplot2 plot object
make_power_topoplot <- function(psd_data, topo_bins = 6, freq_min = -Inf, freq_max = Inf) {

  # Reference: https://craddm.github.io/eegUtils/articles/topoplot/topoplot.html
  psd_data %>%
    dplyr::inner_join(eegUtils:::electrodeLocs, by = c("channel" = "electrode")) %>%
    # Need to filter here down to alpha business
    dplyr::filter(dplyr::between(frequency, freq_min, freq_max)) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = x,
                 y = y,
                 fill = power,
                 z = power) +
    eegUtils::geom_topo(color = "black",
                        bins = topo_bins,
                        interp_limit = "head") +
    ggplot2::scale_fill_gradientn(colours = rainbow(10)) +
    ggplot2::theme_void() +
    ggplot2::coord_equal() +
    ggplot2::labs(fill = expression(paste("Power [", mu, V^2, "/ Hz(dB)]")))


}





#' Function to calculate mean asymmetry
#'
#' @param psd_data df data from `make_psd_data()`
#' @param freq_min minimum frequency to include in calculations
#' @param freq_max minimum frequency to include in calculations
#' @param channel_right right-side (numerator) channel for calculation
#' @param channel_left left-side (denominator) channel for calculation
#'
#' @return single asymmetry value
calculate_asymmetry <- function(psd_data, freq_min, freq_max, channel_right, channel_left) {

  psd_data %>%
    # frequency range and make longer
    dplyr::filter(dplyr::between(frequency, freq_min, freq_max)) %>%
    tidyr::pivot_wider(names_from = channel, values_from = power) %>%
    # calculate ln(right/left)
    dplyr::mutate(asymmetry = log({{channel_right}}/{{channel_left}})) %>%
    # take the mean and pull out
    dplyr::summarize(mean_asymmetry = mean(asymmetry)) %>%
    dplyr::pull(mean_asymmetry)
}



#' Function to make FAA tibble asymmetry from pre-designated electrode pairs
#'
#' @param psd_data df data from `make_psd_data()`
#'
#' @return tibble of FAAs to display in app
make_faa_tibble <- function(psd_data) {
   tibble::tribble(
     ~`Channels`, ~`FAA`,
     "F7-F8", calculate_asymmetry(psd_data, alpha_freq_min, alpha_freq_max, F8, F7),
     "AF3-AF4", calculate_asymmetry(psd_data, alpha_freq_min, alpha_freq_max, AF4, AF3),
     "F3-F4", calculate_asymmetry(psd_data, alpha_freq_min, alpha_freq_max, F4, F3)
   )
}
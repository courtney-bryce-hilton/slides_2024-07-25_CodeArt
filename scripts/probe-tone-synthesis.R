#
# generate probe tones for the tonal hierarchies demo
#
# libraries ---------------------------------------------------------------


library(here)
library(tidyverse)
library(melody) # this is my own package, see Courtney for installation


# define functions for amplitude envelope ---------------------------------


# Function to create a smooth amplitude envelope
create_amplitude_envelope <- function(total_length, fade_in_proportion, fade_out_proportion) {
  # Create the fade-in section
  fade_in_length <- round(total_length * fade_in_proportion)
  fade_in <- 0.5 * (1 - cos(pi * seq(0, 1, length.out = fade_in_length)))
  
  # Create the steady state section
  steady_state_length <- total_length - fade_in_length - round(total_length * fade_out_proportion)
  steady_state <- rep(1, steady_state_length)
  
  # Create the fade-out section
  fade_out_length <- total_length - fade_in_length - steady_state_length
  fade_out <- 0.5 * (1 + cos(pi * seq(0, 1, length.out = fade_out_length)))
  
  # Combine the sections
  envelope <- c(fade_in, steady_state, fade_out)
  
  return(envelope)
}

# Parameters
total_length <- 100  # Total length of the envelope
fade_in_proportion <- 0.4  # Proportion of the envelope for fade-in
fade_out_proportion <- 0.6  # Proportion of the envelope for fade-out

# Create the amplitude envelope
amplitude_envelope <- create_amplitude_envelope(total_length, fade_in_proportion, fade_out_proportion)


# delete any old probe files ----------------------------------------------


# list any old probe files
old_files <- list.files(here("media", "audio", "probes"), pattern = ".mp3", full.names = TRUE)

# delete old probe files
walk(old_files, file.remove)


# synthesise the probe tones ----------------------------------------------


# synthesise all 12 probe pitches 12TET
walk(seq(0,11), \(.pitch) {
  synth(
    pitches = .pitch,
    beat_duration = 3,
    synth_engine = puretone_synth,
    filename = here("media", "audio", "probes", paste0("probe_", sprintf("%02d", .pitch))),
    loudness = 0.5,
    freq = 200,
    fadeout = FALSE,
    amplitude_envelope = amplitude_envelope
  )
})

# chord stimulus
synth(
  pitches = list(c(0,4,7)),
  beat_duration = 3,
  synth_engine = puretone_synth,
  filename = here("media", "audio", "0_4_7"),
  loudness = 0.5,
  freq = 200,
  fadeout = FALSE,
  amplitude_envelope = amplitude_envelope
)


# Parameters
total_length <- 100  # Total length of the envelope
fade_in_proportion <- 0.6  # Proportion of the envelope for fade-in
fade_out_proportion <- 0.4  # Proportion of the envelope for fade-out

# Create the amplitude envelope
amplitude_envelope <- create_amplitude_envelope(total_length, fade_in_proportion, fade_out_proportion)

# congruent example
synth(
  pitches = c(0,2,4,5,7,9,11, "silence", 12),
  rhythms = c(rep(1,7), 2, 4),
  beat_duration = 0.5,
  synth_engine = puretone_synth,
  filename = here("media", "audio", "congruent"),
  loudness = 0.5,
  freq = 200,
  fadeout = FALSE,
  amplitude_envelope = amplitude_envelope
)

synth(
  pitches = c(0,2,4,5,7,9,11, "silence", 6),
  rhythms = c(rep(1,7), 2, 4),
  beat_duration = 0.5,
  synth_engine = puretone_synth,
  filename = here("media", "audio", "incongruent"),
  loudness = 0.5,
  freq = 200,
  fadeout = FALSE,
  amplitude_envelope = amplitude_envelope
)

# -------------------------------------------------------------------------
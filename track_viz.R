

# Install necessary packages if they are not already installed
# Load the libraries
library(tuneR)
library(ggplot2)
library(here)
library(patchwork)

# Reading the MP3 file
# Replace 'your_song.mp3' with the path to your actual MP3 file
song <- readMP3(here('data', 'Cut_1967_7__12s.mp3'))

# Convert the Wave object to a data frame for plotting
# We will only plot the first channel if it's stereo
wave_data <- data.frame(
  time = seq(0, length(song@left) - 1) / song@samp.rate,
  amplitude = song@left
)

# Plot the sound wave using ggplot
ggplot(wave_data, aes(x = time, y = amplitude)) +
  geom_line() +
  ylim(c(-22000, 22000)) +
  labs(
       x = 'Time (seconds)',
       y = 'Amplitude') +
  theme_minimal()


ggsave(filename = "track_example.png", path = here("media", "figures"), width = 6, height = 3)


# Reading the MP3 file
# Replace 'your_song.mp3' with the path to your actual MP3 file
song <- readMP3(here('data', 'probe_00.mp3'))

# Convert the Wave object to a data frame for plotting
# We will only plot the first channel if it's stereo
wave_data <- data.frame(
  time = seq(0, length(song@left) - 1) / song@samp.rate,
  amplitude = song@left
)

# Plot the sound wave using ggplot
ggplot(wave_data, aes(x = time, y = amplitude)) +
  geom_line() +
  ylim(c(-22000, 22000)) +
  labs(
    x = 'Time (seconds)',
    y = '') +
  theme_minimal()


a + b


ggsave(filename = "probe_example.png", path = here("media", "figures"), width = 2, height = 3)


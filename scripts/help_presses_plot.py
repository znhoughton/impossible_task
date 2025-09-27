import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# This should be the folder containing  "all_dogs_cleaned_master.csv"
data_folder = "/data"

# LOAD DATA
file_path = f"{data_folder}/all_dogs_cleaned_master.csv"

try:
    all_data = pd.read_csv(file_path)
except FileNotFoundError:
    print(f"ERROR: The file was not found at {file_path}")
    print("Please make sure the 'data_folder' variable is set correctly.")
    # Exit the script if the file isn't found
    exit()


# DATA PREPARATION

all_data = all_data[all_data['Observation id'] != 'ImpossibleTask']

# Define the window of interest (last START to last STOP)
starts = all_data[all_data['Behavior'] == 'Start'].groupby('Observation id')['Start (s)'].max()
stops = all_data[all_data['Behavior'] == 'End'].groupby('Observation id')['Start (s)'].max()
window = pd.DataFrame({'window_start': starts, 'window_end': stops}).reset_index()

window.loc[window['Observation id'] == 'Hugo', 'window_start'] = 1000.518

# Normalize all event times relative to the start of that window 
merged_data = pd.merge(all_data, window, on='Observation id', how='left')
final_phase_data = merged_data[
    (merged_data['Start (s)'] >= merged_data['window_start']) &
    (merged_data['Stop (s)'] <= merged_data['window_end'])
].copy()
final_phase_data['start_norm'] = final_phase_data['Start (s)'] - final_phase_data['window_start']
final_phase_data['stop_norm'] = final_phase_data['Stop (s)'] - final_phase_data['window_end'] # This was previously 'window_start', changed to 'window_end' - assuming normalized stop should also be relative to window start
final_phase_data['stop_norm'] = final_phase_data['Stop (s)'] - final_phase_data['window_start']


# Prepare data layers using the normalized times 
tupperware_interaction = final_phase_data[final_phase_data['Behavior'] == 'interacting with tupperware']
soundboard_press = final_phase_data[final_phase_data['Behavior'] == 'soundboard press']
window['duration'] = window['window_end'] - window['window_start']


# PLOTTING
dog_names = sorted(final_phase_data['Observation id'].unique(), reverse=True)
y_positions = np.arange(len(dog_names))
dog_to_y = {dog: pos for pos, dog in enumerate(dog_names)}

fig, ax = plt.subplots(figsize=(14, 10))

for dog, y_pos in dog_to_y.items():
    # Layer 1: The new blue timeline
    window_data = window[window['Observation id'] == dog]
    if not window_data.empty:
        ax.hlines(y=y_pos, xmin=0, xmax=window_data['duration'].iloc[0], color='steelblue', linewidth=2, zorder=2)

    # Layer 2: Green Tupperware interactions
    tupperware_data = tupperware_interaction[tupperware_interaction['Observation id'] == dog]
    for _, row in tupperware_data.iterrows():
        ax.hlines(y=y_pos, xmin=row['start_norm'], xmax=row['stop_norm'], color='darkgreen', linewidth=4, zorder=3)
        
    # Layer 3: Soundboard presses (with conditional coloring)
    press_data = soundboard_press[soundboard_press['Observation id'] == dog].copy()
    
    # Default to grey for all presses first
    press_data['color'] = 'grey'
    
    # Color Teega's press at 154s
    if dog == 'Teega':
        # Use a small range to account for floating point numbers
        press_data.loc[(press_data['start_norm'] > 152) & (press_data['start_norm'] < 160), 'color'] = 'red'

    # Color Parker's "HELP" presses
    if dog == 'Parker':
        # Convert times to seconds: 0:52=52s, 2:44=164s, 2:53=173s
        # CORRECTED: Ensures the 52s press is red, which is the first diamond in the plot
        parker_red_times = [52, 165, 173] 
        for time in parker_red_times:
             # Use a small range around the target time
            press_data.loc[(press_data['start_norm'] > time - 0.7) & (press_data['start_norm'] < time + 0.5), 'color'] = 'red'

    # Plot the soundboard presses using the assigned colors
    ax.scatter(press_data['start_norm'], [y_pos] * len(press_data), color=press_data['color'], marker='d', s=50, zorder=4)

ax.set_yticks(y_positions)
ax.set_yticklabels(dog_names)
ax.set_xlabel('Time (seconds) Since Start of Test', fontsize=12)
ax.set_ylabel('Dog', fontsize=12)
ax.set_title('Tupperware Interactions and Button Presses in Unsolvable Trial', fontsize=16, fontweight='bold') # Changed title slightly for clarity based on plot
ax.grid(axis='x', linestyle='--', alpha=0.6)
ax.spines['top'].set_visible(False)
ax.spines['right'].set_visible(False)
ax.spines['left'].set_visible(False)

plt.tight_layout()

# DISPLAY AND SAVE THE PLOT
plt.savefig("final_plot_help_presses.png", dpi=300)
print("Final plot saved as 'final_plot_help_presses.png'")
plt.show()
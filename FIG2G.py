import pandas as pd

import matplotlib.pyplot as plt
import seaborn as sns
import warnings

warnings.filterwarnings("ignore")

plt.rcParams.update({
    'font.family': 'sans-serif',
    'font.sans-serif': ['Arial', 'DejaVu Sans'],
    'font.size': 9,
    'axes.titlesize': 12,
    'axes.labelsize': 10,
    'axes.linewidth': 0.8,
    'xtick.labelsize': 8,
    'ytick.labelsize': 8,
    'figure.dpi': 300,
    'savefig.dpi': 600
})

df = pd.read_csv(
    r"D:\heathycare data\revised_figures\figure2_sem_effects.csv"
)

df['Significant'] = (
    df['Significant']
    .astype(str)
    .str.strip()
    .replace({'*': 1, 'nan': 0, 'None': 0, '': 0})
    .astype(int)
)

df['X_label'] = (
    df['Group2'].astype(str) + ' | ' +
    df['Gene'].astype(str)
)

df['Y_label'] = (
    df['Group'].astype(str) + ' | ' +
    df['name'].astype(str)
)

df = df.sort_values(
    by=['Group', 'name', 'Group2', 'Gene'],
    ascending=[True, True, True, True]
)

heatmap_data = df.pivot_table(
    index='Y_label',
    columns='X_label',
    values='value',
    aggfunc='mean'
)

sig_data = df.pivot_table(
    index='Y_label',
    columns='X_label',
    values='Significant',
    aggfunc='max'
)

A4_WIDTH = 8.27
cell_size = A4_WIDTH / heatmap_data.shape[1]
fig_height = cell_size * heatmap_data.shape[0] + 1.5

fig, ax = plt.subplots(figsize=(A4_WIDTH, fig_height))

nature_cmap = sns.diverging_palette(240, 10, as_cmap=True)

sns.heatmap(
    heatmap_data,
    ax=ax,
    cmap=nature_cmap,
    center=0,
    square=True,
    linewidths=0.5,
    linecolor='white',
    cbar_kws={
        'label': 'Effect Size',
        'shrink': 0.6
    }
)

for i in range(sig_data.shape[0]):
    for j in range(sig_data.shape[1]):
        if sig_data.iloc[i, j] == 1:
            ax.text(
                j + 0.5,
                i + 0.5,
                '*',
                ha='center',
                va='center',
                fontsize=9,
                fontweight='bold',
                color='black'
            )

x_groups = df[['Group2', 'X_label']].drop_duplicates()
x_counts = x_groups.groupby('Group2').size().cumsum()

for xc in x_counts[:-1]:
    ax.axvline(xc, color='black', linewidth=1.2)

y_groups = df[['Group', 'Y_label']].drop_duplicates()
y_counts = y_groups.groupby('Group').size().cumsum()

for yc in y_counts[:-1]:
    ax.axhline(yc, color='black', linewidth=1.2)

ax.set_xlabel('')
ax.set_ylabel('')

ax.tick_params(axis='x', rotation=90)
ax.tick_params(axis='y', rotation=0)

ax.set_title(
    'Grouped Heatmap of Health Effects',
    fontsize=13,
    fontweight='bold',
    pad=16
)

plt.tight_layout()

plt.savefig(
    r"D:\heathycare data\revised_figures\figure2_sem_effects.pdf",
    bbox_inches='tight',
    facecolor='white'
)

plt.show()

print("Grouped heatmap generated successfully.")
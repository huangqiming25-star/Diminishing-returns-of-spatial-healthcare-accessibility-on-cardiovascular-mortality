import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import warnings

warnings.filterwarnings('ignore')

plt.rcParams.update({
    'font.family': 'sans-serif',
    'font.sans-serif': ['Arial', 'DejaVu Sans'],
    'font.size': 9,
    'axes.titlesize': 11,
    'axes.labelsize': 10,
    'axes.linewidth': 0.8,
    'axes.labelweight': 'bold',
    'xtick.labelsize': 8,
    'ytick.labelsize': 8,
    'figure.dpi': 300,
    'savefig.dpi': 600,
    'figure.figsize': (8.5, 5),
    'axes.grid': True,
    'grid.alpha': 0.2,
    'grid.linestyle': '--',
    'grid.linewidth': 0.5
})

df = pd.read_csv(r"D:\heathycare data\picture2\marginal_effect2.csv")

df = df[(df['year'] >= 2013) & (df['year'] <= 2023)].copy()

records = []

for c in df['Country'].unique():
    d = df[df['Country'] == c]

    records.append({
        'Country': c,
        'Continent': d['Continent'].iloc[0],
        'avg_all': d['all'].mean(),
        'avg_cardio': d['Cardiovascular.diseases'].mean(),
        'avg_de_da': np.abs(d['dE_dA_mean']).mean()
    })

df_avg = pd.DataFrame(records)

x_median = df_avg['avg_cardio'].median()
y_median = df_avg['avg_de_da'].median()

def assign_group(row):
    if row['avg_cardio'] >= x_median and row['avg_de_da'] >= y_median:
        return 'HH'
    elif row['avg_cardio'] >= x_median and row['avg_de_da'] < y_median:
        return 'HL'
    elif row['avg_cardio'] < x_median and row['avg_de_da'] >= y_median:
        return 'LH'
    else:
        return 'LL'

df_avg['group'] = df_avg.apply(assign_group, axis=1)

group_colors = {
    'HH': "#D62828",
    'HL': "#6D488E",
    'LH': "#0E693A",
    'LL': "#fc7c1f"
}

q = df_avg['avg_all'].quantile([0.25, 0.5, 0.75])

def size_map(x):
    if x <= q[0.25]:
        return 30
    elif x <= q[0.5]:
        return 60
    elif x <= q[0.75]:
        return 80
    else:
        return 110

df_avg['point_size'] = df_avg['avg_all'].apply(size_map)

fig, ax = plt.subplots()

x_min, x_max = df_avg['avg_cardio'].min(), df_avg['avg_cardio'].max()
y_min, y_max = df_avg['avg_de_da'].min(), df_avg['avg_de_da'].max()

ax.set_xlim(x_min * 0.95, x_max * 1.05)
ax.set_ylim(y_min * 0.95, y_max * 1.05)

bg_colors = ['#FFF0F0', '#F0F0FF', '#F0FFF0', '#FFF8F0']

rects = [
    patches.Rectangle((x_median, y_median), x_max, y_max,
                      facecolor=bg_colors[0], alpha=0.15),

    patches.Rectangle((x_min, y_median), x_median - x_min, y_max,
                      facecolor=bg_colors[1], alpha=0.15),

    patches.Rectangle((x_min, y_min), x_median - x_min, y_median - y_min,
                      facecolor=bg_colors[2], alpha=0.15),

    patches.Rectangle((x_median, y_min), x_max, y_median - y_min,
                      facecolor=bg_colors[3], alpha=0.15)
]

for r in rects:
    ax.add_patch(r)

ax.axvline(x_median, color='gray', linestyle='--', linewidth=1)
ax.axhline(y_median, color='gray', linestyle='--', linewidth=1)

for g, color in group_colors.items():
    d = df_avg[df_avg['group'] == g]

    ax.scatter(
        d['avg_cardio'],
        d['avg_de_da'],
        s=d['point_size'],
        c=color,
        marker='o',
        edgecolors='white',
        linewidth=0.7,
        alpha=0.85,
        zorder=2
    )

for g in df_avg['group'].unique():
    d = df_avg[df_avg['group'] == g]

    reps = pd.concat([
        d.nlargest(3, 'avg_all'),
        d.nsmallest(3, 'avg_all')
    ]).drop_duplicates()

    for _, row in reps.iterrows():
        ax.annotate(
            row['Country'],
            xy=(row['avg_cardio'], row['avg_de_da']),
            xytext=(10, 10),
            textcoords='offset points',
            fontsize=7,
            ha='left',
            va='bottom',
            bbox=dict(
                boxstyle='round,pad=0.2',
                facecolor='white',
                alpha=0.9,
                edgecolor=group_colors[g],
                linewidth=0.6
            ),
            arrowprops=dict(
                arrowstyle='-',
                linestyle='--',
                color=group_colors[g],
                linewidth=0.6,
                alpha=0.8,
                shrinkA=0,
                shrinkB=2
            )
        )

ax.set_xlabel(
    'Average Cardiovascular Disease Burden (2013–2023)',
    fontsize=11,
    fontweight='bold'
)

ax.set_ylabel(
    'Average |dE/dA| (2013–2023)',
    fontsize=11,
    fontweight='bold'
)

ax.set_title(
    'Country-Level Health Metrics (2013–2023 Mean)\n'
    'Joint High–Low Classification of Disease Burden and Sensitivity',
    fontsize=13,
    fontweight='bold',
    pad=15
)

ax.text(
    0.99, 0.01,
    'Color: HH / HL / LH / LL\n'
    'Circle size: all-cause mortality',
    transform=ax.transAxes,
    ha='right',
    va='bottom',
    fontsize=7,
    alpha=0.7
)

plt.tight_layout()

plt.savefig(
    r'D:\heathycare data\picture2b0430.pdf',
    bbox_inches='tight',
    facecolor='white'
)

plt.show()

df_avg.to_csv(
    r'D:\heathycare data\picture2b0430.csv',
    index=False
)

print("Analysis complete. Figure saved.")
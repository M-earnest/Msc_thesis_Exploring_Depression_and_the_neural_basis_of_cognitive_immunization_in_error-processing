#ToDo:
#    - baseline
#    - fine-tune parameters
#    - group-lvl plots?

#adapted
#from https: // mne.tools / stable / auto_tutorials / time - freq / 20
#_sensors_time_frequency.html

##adapted from authors: Alexandre
#Gramfort < alexandre.gramfort @ inria.fr >
#Stefan
#Appelhoff < stefan.appelhoff @ mailbox.org >
#Richard
#Höchenberger < richard.hoechenberger @ gmail.com >
import os
import os.path as op
import pandas as pd
import numpy as np

import mne
from mne.time_frequency import tfr_morlet, psd_multitaper, psd_welch
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from mpl_toolkits.axes_grid1 import make_axes_locatable
from matplotlib.colorbar import ColorbarBase
from matplotlib.colors import Normalize
import seaborn as sns
from matplotlib.colors import ListedColormap

from mne import read_epochs, combine_evoked, grand_average
from mne.channels import make_1020_channel_selections
from mne.viz import plot_compare_evokeds

pos_incongruent_incorrect_base = dict()
pos_incongruent_incorrect_erps_base = dict()
pos_incongruent_incorrect_pos = dict()
pos_incongruent_incorrect_erps_pos = dict()
pos_incongruent_incorrect_neg = dict()
pos_incongruent_incorrect_erps_neg = dict()

neg_incongruent_incorrect_base = dict()
neg_incongruent_incorrect_erps_base = dict()
neg_incongruent_incorrect_pos = dict()
neg_incongruent_incorrect_erps_pos = dict()
neg_incongruent_incorrect_neg = dict()
neg_incongruent_incorrect_erps_neg = dict()

rootdir = '/home/michael/data/derivatives/reaction_epochs'

counter = 0
directory = '/home/michael/data/derivatives/results/erp_plots'

if not os.path.exists(directory):
    os.makedirs(directory)
# read epoch data and merge
for subdir, dirs, files in os.walk(rootdir):
    for file in files:
        filepath = subdir + os.sep + file
        if filepath.endswith(".fif"):
            print (filepath)
            print((filepath.split('/')[-2].split('-')[-1]))
            subject = (filepath.split('/')[-2].split('-')[-1])[-2:]
            target_epo = read_epochs(filepath, preload=True)

            display(target_epo.metadata['negative_first'].iloc[0])
            # split by group membership
            if target_epo.metadata['negative_first'].iloc[0] == True:
                display('########################')
                display(target_epo.metadata['negative_first'].iloc[0])
                display('sorted to negative first')
                display('########################')
                # and apply baseline
                neg_incongruent_incorrect_base['subj_' + subject] = target_epo['condition == 1']['incorrect_incongruent'].apply_baseline((-0.300, -0.050))
                neg_incongruent_incorrect_pos['subj_' + subject] = target_epo['condition == 2']['incorrect_incongruent'].apply_baseline((-0.300, -0.050))
                neg_incongruent_incorrect_neg['subj_' + subject] = target_epo['condition == 3']['incorrect_incongruent'].apply_baseline((-0.300, -0.050))

                # compute ERP
                neg_incongruent_incorrect_erps_base['subj_' + subject] = neg_incongruent_incorrect_base['subj_' + subject].average()
                neg_incongruent_incorrect_erps_pos['subj_' + subject] = neg_incongruent_incorrect_pos['subj_' + subject].average()
                neg_incongruent_incorrect_erps_neg['subj_' + subject] = neg_incongruent_incorrect_neg['subj_' + subject].average()
                #display(incongruent_incorrect_erps_neu)
            elif target_epo.metadata['negative_first'].iloc[0] == False:
                display('########################')
                display(target_epo.metadata['negative_first'].iloc[0])
                display('sorted to positive first')
                display('########################')
                # and apply baseline
                pos_incongruent_incorrect_base['subj_' + subject] = target_epo['condition == 1']['incorrect_incongruent'].apply_baseline((-0.300, -0.050))
                pos_incongruent_incorrect_pos['subj_' + subject] = target_epo['condition == 2']['incorrect_incongruent'].apply_baseline((-0.300, -0.050))
                pos_incongruent_incorrect_neg['subj_' + subject] = target_epo['condition == 3']['incorrect_incongruent'].apply_baseline((-0.300, -0.050))

                # compute ERP
                pos_incongruent_incorrect_erps_base['subj_' + subject] = pos_incongruent_incorrect_base['subj_' + subject].average()
                pos_incongruent_incorrect_erps_pos['subj_' + subject] = pos_incongruent_incorrect_pos['subj_' + subject].average()
                pos_incongruent_incorrect_erps_neg['subj_' + subject] = pos_incongruent_incorrect_neg['subj_' + subject].average()

power_df_pos = pd.DataFrame(columns=['FCz_power', 'Cz_power', 'FCz_itc', 'Cz_itc',
                                     'soc_condition', 'group', 'subject'])

# extract theta and write to df
for i, j in enumerate(pos_incongruent_incorrect_erps_base.keys()):
    print(i)
    print(j)
    pos_incon_incorr_epochs_base = pos_incongruent_incorrect_base[j]
    pos_incon_incorr_epochs_pos = pos_incongruent_incorrect_pos[j]
    pos_incon_incorr_epochs_neg = pos_incongruent_incorrect_neg[j]

    #pos_incon_incorr_epochs_base.plot_psd_topomap(ch_type='eeg', normalize=True);
    #pos_incon_incorr_epochs_pos.plot_psd_topomap(ch_type='eeg', normalize=True);
    #pos_incon_incorr_epochs_neg.plot_psd_topomap(ch_type='eeg', normalize=True);

    #pos_incon_incorr_epochs_base.plot_psd(fmin=2., fmax=40., average=True, spatial_colors=False);
    #pos_incon_incorr_epochs_neg.plot_psd(fmin=2., fmax=40., average=True, spatial_colors=False);
    #pos_incon_incorr_epochs_pos.plot_psd(fmin=2., fmax=40., average=True, spatial_colors=False);

    # baseline

    # define define frequencies of interest (log-spaced) using morlet wave
    freqs = np.logspace(*np.log10([3, 10]), num=7)
    n_cycles = freqs / 2.  # different number of cycle per frequency
    power, itc = tfr_morlet(pos_incon_incorr_epochs_base, freqs=freqs, n_cycles=n_cycles, use_fft=True,
                            return_itc=True, decim=3, n_jobs=1)

   # power.plot_topo(baseline=(-0.5, 0), mode='logratio', title='Average power')
    #power.plot([-18], baseline=(-0.5, 0), mode='logratio', title=power.ch_names[-18])

    #fig, axis = plt.subplots(1, 2, figsize=(7, 4))
    #power.plot_topomap(ch_type='eeg', tmin=0.5, tmax=1.5, fmin=3, fmax=9,
    #                   baseline=(-0.5, 0), mode='logratio', axes=axis[0],
    #                   title='theta', show=False)
    #mne.viz.tight_layout()
    #plt.show()

    #print('baseline')
    #itc.plot_topo(title='Inter-Trial coherence', vmin=-1., vmax=1., cmap='Reds');

    # data to dataframe
    # power_df_temp = power.to_data_frame()
    power_df_temp = pd.DataFrame()
    power_df_temp['FCz_power'] = power.to_data_frame()['FCz']
    power_df_temp['Cz_power'] = power.to_data_frame()['Cz']
    power_df_temp['FCz_itc'] = itc.to_data_frame()['FCz']
    power_df_temp['Cz_itc'] = itc.to_data_frame()['Cz']
    power_df_temp['soc_condition'] = 'baseline'
    power_df_temp['group'] = 'positive_first'
    power_df_temp['subject'] = subject
    power_df_temp['time'] = power.to_data_frame()['time']
    power_df_temp['freq'] = power.to_data_frame()['freq']
    power_df_pos = power_df_pos.append(power_df_temp)

    # negative

    # define define frequencies of interest (log-spaced) using morlet wave
    freqs = np.logspace(*np.log10([3, 10]), num=10)
    n_cycles = freqs / 2.  # different number of cycle per frequency
    power, itc = tfr_morlet(pos_incon_incorr_epochs_neg, freqs=freqs, n_cycles=n_cycles, use_fft=True,
                            return_itc=True, decim=3, n_jobs=1)

    #power.plot_topo(baseline=(-0.5, 0), mode='logratio', title='Average power')
    #power.plot([-18], baseline=(-0.5, 0), mode='logratio', title=power.ch_names[-18])

    #fig, axis = plt.subplots(1, 2, figsize=(7, 4))
    #power.plot_topomap(ch_type='eeg', tmin=0.5, tmax=1.5, fmin=3, fmax=9,
    #                   baseline=(-0.5, 0), mode='logratio', axes=axis[0],
    #                   title='theta', show=False)
    #mne.viz.tight_layout()
    #plt.show()

    print('negative')
    #itc.plot_topo(title='Inter-Trial coherence', vmin=-1., vmax=1., cmap='Reds');
    # data to dataframe
    power_df_temp = pd.DataFrame()
    power_df_temp['FCz_power'] = power.to_data_frame()['FCz']
    power_df_temp['Cz_power'] = power.to_data_frame()['Cz']
    power_df_temp['FCz_itc'] = itc.to_data_frame()['FCz']
    power_df_temp['Cz_itc'] = itc.to_data_frame()['Cz']
    power_df_temp['soc_condition'] = 'negative'
    power_df_temp['group'] = 'positive_first'
    power_df_temp['subject'] = subject
    power_df_temp['time'] = power.to_data_frame()['time']
    power_df_temp['freq'] = power.to_data_frame()['freq']

    power_df_pos = power_df_pos.append(power_df_temp)
    # positive

    # define define frequencies of interest (log-spaced) using morlet wave
    freqs = np.logspace(*np.log10([3, 10]), num=10)
    n_cycles = freqs / 2.  # different number of cycle per frequency
    power, itc = tfr_morlet(pos_incon_incorr_epochs_pos, freqs=freqs, n_cycles=n_cycles, use_fft=True,
                            return_itc=True, decim=3, n_jobs=1)

   # power.plot_topo(baseline=(-0.5, 0), mode='logratio', title='Average power')
    #power.plot([-18], baseline=(-0.5, 0), mode='logratio', title=power.ch_names[-18])

    #fig, axis = plt.subplots(1, 2, figsize=(7, 4))
    #power.plot_topomap(ch_type='eeg', tmin=0.5, tmax=1.5, fmin=3, fmax=9,
    #                   baseline=(-0.5, 0), mode='logratio', axes=axis[0],
    #                   title='theta', show=False)
    #mne.viz.tight_layout()
    #plt.show()

    power_df_temp = pd.DataFrame()
    power_df_temp['FCz_power'] = power.to_data_frame()['FCz']
    power_df_temp['Cz_power'] = power.to_data_frame()['Cz']
    power_df_temp['FCz_itc'] = itc.to_data_frame()['FCz']
    power_df_temp['Cz_itc'] = itc.to_data_frame()['Cz']
    power_df_temp['soc_condition'] = 'positive'
    power_df_temp['group'] = 'positive_first'
    power_df_temp['subject'] = subject
    power_df_temp['time'] = power.to_data_frame()['time']
    power_df_temp['freq'] = power.to_data_frame()['freq']
    power_df_pos = power_df_pos.append(power_df_temp)


#### negative first group

power_df_neg = pd.DataFrame(columns=['FCz_power', 'Cz_power', 'FCz_itc', 'Cz_itc',
                                     'soc_condition', 'group', 'subject'])

for i, j in enumerate(neg_incongruent_incorrect_erps_base.keys()):
    print(i)
    print(j)

    neg_incon_incorr_epochs_base = neg_incongruent_incorrect_base[j]
    neg_incon_incorr_epochs_pos = neg_incongruent_incorrect_pos[j]
    neg_incon_incorr_epochs_neg = neg_incongruent_incorrect_neg[j]

    # topo plots
    #print('baseline')
    #neg_incon_incorr_epochs_base.plot_psd_topomap(ch_type='eeg', normalize=True);
    #print('negative')
    #neg_incon_incorr_epochs_pos.plot_psd_topomap(ch_type='eeg', normalize=True);
    #print('positive')
    #neg_incon_incorr_epochs_neg.plot_psd_topomap(ch_type='eeg', normalize=True);

    ## Spectral info plots

    #print('baseline')
    #neg_incon_incorr_epochs_base.plot_psd(fmin=2., fmax=40., average=True, spatial_colors=False);
    #print('negative')
    #neg_incon_incorr_epochs_neg.plot_psd(fmin=2., fmax=40., average=True, spatial_colors=False);
    #print('positive')
    #neg_incon_incorr_epochs_pos.plot_psd(fmin=2., fmax=40., average=True, spatial_colors=False);

    ## PSD plots
    #f, ax = plt.subplots()
    #psds, freqs = psd_multitaper(neg_incon_incorr_epochs_neg, fmin=2, fmax=40, n_jobs=1)
    #psds = 10. * np.log10(psds)
    #psds_mean = psds.mean(0).mean(0)
    #psds_std = psds.mean(0).std(0)

    #ax.plot(freqs, psds_mean, color='k')
    #ax.fill_between(freqs, psds_mean - psds_std, psds_mean + psds_std,
    #                color='k', alpha=.5)
    #ax.set(title='Pos: Multitaper PSD (gradiometers)', xlabel='Frequency (Hz)',
    #       ylabel='Power Spectral Density (dB)')
    #plt.show()


    # positive

    # Estimate PSDs based on "mean" and "median" averaging for comparison.
    #kwargs = dict(fmin=2, fmax=40, n_jobs=1)
    #psds_welch_mean, freqs_mean = psd_welch(neg_incon_incorr_epochs_pos, average='mean', **kwargs)
    #psds_welch_median, freqs_median = psd_welch(neg_incon_incorr_epochs_pos, average='median', **kwargs)

    # Convert power to dB scale.
    #psds_welch_mean = 10 * np.log10(psds_welch_mean)
    #psds_welch_median = 10 * np.log10(psds_welch_median)

    # We will only plot the PSD for a single sensor in the first epoch.
    #ch_name = 'FCz'
    #ch_idx = neg_incon_incorr_epochs_pos.info['ch_names'].index(ch_name)
    #epo_idx = 0

    #_, ax = plt.subplots()
    #ax.plot(freqs_mean, psds_welch_mean[epo_idx, ch_idx, :], color='k',
    #        ls='-', label='mean of segments')
    #ax.plot(freqs_median, psds_welch_median[epo_idx, ch_idx, :], color='k',
    #        ls='--', label='median of segments')

    #ax.set(title='Pos: Welch PSD ({}, Epoch {})'.format(ch_name, epo_idx),
    #       xlabel='Frequency (Hz)', ylabel='Power Spectral Density (dB)')
    #ax.legend(loc='upper right')
    #plt.show()

    # mortlet time-freq representation

    # baseline

    # define define frequencies of interest (log-spaced) using morlet wave
    freqs = np.logspace(*np.log10([3, 10]), num=15)
    n_cycles = freqs / 2.  # different number of cycle per frequency
    power, itc = tfr_morlet(neg_incon_incorr_epochs_base, freqs=freqs, n_cycles=n_cycles, use_fft=True,
                            return_itc=True, decim=3, n_jobs=1)

    # data to dataframe
    # power_df_temp = power.to_data_frame()
    power_df_temp = pd.DataFrame()
    power_df_temp['FCz_power'] = power.to_data_frame()['FCz']
    power_df_temp['Cz_power'] = power.to_data_frame()['Cz']
    power_df_temp['FCz_itc'] = itc.to_data_frame()['FCz']
    power_df_temp['Cz_itc'] = itc.to_data_frame()['Cz']
    power_df_temp['soc_condition'] = 'negative'
    power_df_temp['group'] = 'negative_first'
    power_df_temp['subject'] = subject
    power_df_temp['time'] = power.to_data_frame()['time']
    power_df_temp['freq'] = power.to_data_frame()['freq']
    power_df_neg = power_df_neg.append(power_df_temp)

    # negative

    # define define frequencies of interest (log-spaced) using morlet wave
    freqs = np.logspace(*np.log10([3, 10]), num=10)
    n_cycles = freqs / 2.  # different number of cycle per frequency
    power, itc = tfr_morlet(neg_incon_incorr_epochs_neg, freqs=freqs, n_cycles=n_cycles, use_fft=True,
                            return_itc=True, decim=3, n_jobs=1)


    # data to dataframe
    power_df_temp = pd.DataFrame()
    power_df_temp['FCz_power'] = power.to_data_frame()['FCz']
    power_df_temp['Cz_power'] = power.to_data_frame()['Cz']
    power_df_temp['FCz_itc'] = itc.to_data_frame()['FCz']
    power_df_temp['Cz_itc'] = itc.to_data_frame()['Cz']
    power_df_temp['soc_condition'] = 'negative'
    power_df_temp['group'] = 'negative_first'
    power_df_temp['subject'] = subject
    power_df_temp['time'] = power.to_data_frame()['time']
    power_df_temp['freq'] = power.to_data_frame()['freq']

    power_df_neg = power_df_neg.append(power_df_temp)
    # positive

    # define define frequencies of interest (log-spaced) using morlet wave
    freqs = np.logspace(*np.log10([3, 10]), num=10)
    n_cycles = freqs / 2.  # different number of cycle per frequency
    power, itc = tfr_morlet(neg_incon_incorr_epochs_pos, freqs=freqs, n_cycles=n_cycles, use_fft=True,
                            return_itc=True, decim=3, n_jobs=1)

    power_df_temp = pd.DataFrame()
    power_df_temp['FCz_power'] = power.to_data_frame()['FCz']
    power_df_temp['Cz_power'] = power.to_data_frame()['Cz']
    power_df_temp['FCz_itc'] = itc.to_data_frame()['FCz']
    power_df_temp['Cz_itc'] = itc.to_data_frame()['Cz']
    power_df_temp['soc_condition'] = 'positive'
    power_df_temp['group'] = 'negative_first'
    power_df_temp['subject'] = subject
    power_df_temp['time'] = power.to_data_frame()['time']
    power_df_temp['freq'] = power.to_data_frame()['freq']
    power_df_neg = power_df_neg.append(power_df_temp)


power_df = power_df_pos.append(power_df_neg)

power_df.to_csv('/home/michael/git/master_thesis/data/power_itc_long_format.csv', sep='\t')

### or use https://mne.tools/stable/auto_tutorials/time-freq/20_sensors_time_frequency.html
#Frequency and time - frequency
#sensor
#analysis

#from mne.baseline import rescale
#from mne.stats import bootstrap_confidence_interval

# let's explore some frequency bands
#iter_freqs = [
#    ('Theta', 4, 7),
#    ('Alpha', 8, 12),
#    ('Beta', 13, 25),
#    ('Gamma', 30, 45)
#]


# Helper function for plotting spread
#def stat_fun(x):
#    """Return sum of squares."""
#    return np.sum(x ** 2, axis=0)


#for i, j in enumerate(neg_incongruent_incorrect_erps_base.keys()):
#    print(i)
#    print(j)

## Global field power baseline
#print('baseline')

#neg_incon_incorr_epochs_base = neg_incongruent_incorrect_base[j]

#frequency_map = list()
#for band, fmin, fmax in iter_freqs:
    # epochs = mne.Epochs(raw, events, event_id, tmin, tmax, baseline=baseline,
    #                    reject=dict(grad=4000e-13, eog=350e-6),
    #                    preload=True)
    # remove evoked response
#    neg_incon_incorr_epochs_base.subtract_evoked()

    # get analytic signal (envelope)#
    #neg_incon_incorr_epochs_base.apply_hilbert(envelope=True)
#    frequency_map.append(((band, fmin, fmax), neg_incon_incorr_epochs_base.average()))

# Plot
#fig, axes = plt.subplots(4, 1, figsize=(10, 7), sharex=True, sharey=True)
#colors = plt.get_cmap('winter_r')(np.linspace(0, 1, 4))
#for ((freq_name, fmin, fmax), average), color, ax in zip(
#        frequency_map, colors, axes.ravel()[::-1]):
#    times = average.times * 1e3
#    gfp = np.sum(average.data ** 2, axis=0)
#    gfp = mne.baseline.rescale(gfp, times, baseline=(None, 0))
#    ax.plot(times, gfp, label=freq_name, color=color, linewidth=2.5)
#    ax.axhline(0, linestyle='--', color='grey', linewidth=2)
#    ci_low, ci_up = bootstrap_confidence_interval(average.data, random_state=0,
#                                                  stat_fun=stat_fun)
#    ci_low = rescale(ci_low, average.times, baseline=(None, 0))
#    ci_up = rescale(ci_up, average.times, baseline=(None, 0))
#    ax.fill_between(times, gfp + ci_up, gfp - ci_low, color=color, alpha=0.3)
#    ax.grid(True)
#    ax.set_ylabel('GFP')
#    ax.annotate('%s (%d-%dHz)' % (freq_name, fmin, fmax),
#                xy=(0.95, 0.8),
#                horizontalalignment='right',
#                xycoords='axes fraction')
#    ax.set_xlim(-1500, 1500)

#axes.ravel()[-1].set_xlabel('Time [ms]')
#plt.show()

# # extract corrects
# coorect-incongruent, correct-congruent
# correct_epochs = cue_epochs['Correct A', 'Correct B']
# # apply baseline correction
# correct_epochs.apply_baseline((-0.300, -0.050))
#
# # epochs to pandas dataframe
# df = correct_epochs.to_data_frame(long_format=True)
#
# # get time roi N170
# 0 - 100
# channel FCz
# n170 = df[((df["time"] >= 150) & (df["time"] <= 250))
#           & ((df["channel"] == 'PO8') |
#              (df["channel"] == 'PO7') |
#              (df["channel"] == 'FCz'))]
# n170 = n170.assign(subject=subject)
#
# # get time roi LPC
# LPC = df[((df["time"] >= 400) & (df["time"] <= 750))
#          & (df["channel"] == 'Pz')]
# LPC = LPC.assign(subject=subject)
#
#cue_epo = read_epochs(input_file, preload=True)
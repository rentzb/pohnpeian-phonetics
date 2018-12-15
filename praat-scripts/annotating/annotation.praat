# USAGE:
#
# This Praat script makes the process of annotating multiple sound files go
# faster by automating most of the mouse-clicks that are needed to create and
# save new TextGrid files. It takes a folder of sound files. For each sound
# file, the script creates a new TextGrid with the annotation tiers that you
# list in the settings window, then opens that TextGrid along with its
# accompanying sound file. The script pauses while you create annotations for
# the sound file. When you are done creating annotations, press "OK" in the
# pop-up window to save the TextGrid to a file with the same filename as the
# sound file plus a ".TextGrid" extension. After saving the file, the script
# moves on to the next sound file in the folder.
#
# SETTINGS:
#
# Interval tiers:               Provide a list of names for the interval tiers
#                               (if any) that will be created in each new
#                               TextGrid. The tier names should be separated by
#                               spaces. The default will create two interval
#                               tiers named "Mary" and "John".
# Point tiers:                  Provide a list of names for the point tiers
#                               (if any) in each new TextGrid, with the tier
#                               names separated by spaces. The default will
#                               create one point tier named "bell".
# If TextGrid already exists:   If the folder that you select already contains
#                               a TextGrid with the same name as one of the
#                               sound files, what should the script do with
#                               that sound file? The default is to skip that
#                               sound file, or you can also choose to be
#                               prompted for a new TextGrid filename, or to
#                               open the existing TextGrid rather than creating
#                               a new one.
# Sound file extensions:        The extension of the sound files in your folder
#                               (.wav, .flac, etc).
#
# After you configure the settings, press OK to choose the directory of sound
# files to annotate.


form Settings
    sentence Interval_tiers Mary John
    sentence Point_tiers bell
    optionmenu If_TextGrid_already_exists: 1
        option skip the sound file
        option create a TextGrid with a different filename
        option open and edit the existing TextGrid
    word Sound_file_extension .wav
    comment Press OK to choose a directory of sound files to annotate.

endform

directory$ = chooseDirectory$: "Choose a directory with 'sound_file_extension$'
... files to annotate."
@getFiles: directory$, sound_file_extension$

tiers$ = interval_tiers$ + " " + point_tiers$

for i to getFiles.length
    soundfile = Read from file: getFiles.files$ [i]

    @getTextGrid: getFiles.files$ [i]

    if !fileReadable (getTextGrid.path$) or if_TextGrid_already_exists > 1
        selectObject: soundfile, getTextGrid.textgrid
        View & Edit

        beginPause: "Annotation"
            comment: "Press OK when done to save."

        endPause: "OK", 0

        selectObject: getTextGrid.textgrid
        Save as text file: getTextGrid.path$

        removeObject: getTextGrid.textgrid

    endif

    removeObject: soundfile

endfor

procedure getTextGrid: .soundfile$
    .path$ = replace$: .soundfile$, sound_file_extension$, ".TextGrid", 0

    if !fileReadable: .path$
        .textgrid = To TextGrid: tiers$, point_tiers$

    elif if_TextGrid_already_exists == 2
        .textgrid = To TextGrid: tiers$, point_tiers$
        .default$ = mid$: .path$, rindex (.path$, "/") + 1, length (.path$)
        .default$ = replace$: .default$, sound_file_extension$, ".TextGrid", 1

        .path$ = chooseWriteFile$: "TextGrid already exists in this directory. 
        ... Choose where to save the new TextGrid.", .default$

    elif if_TextGrid_already_exists == 3
        .textgrid = Read from file: .path$

    endif

endproc

procedure getFiles: .dir$, .ext$
    .obj = Create Strings as file list: "files", .dir$ + "/*" + .ext$
    .length = Get number of strings

    for .i to .length
        .fname$ = Get string: .i
        .files$ [.i] = .dir$ + "/" + .fname$

    endfor

    removeObject: .obj

endproc

# Importing Instruments into Qualtrics and REDCap

Each instrument’s [download
page](https://jmgirard.github.io/hitop/index.md) offers ready-to-use
import files for the two most common survey platforms,
[Qualtrics](https://www.qualtrics.com) and
[REDCap](https://projectredcap.org). This page collects the step-by-step
instructions for importing those files, so you only have to learn the
process once.

Two different Qualtrics file formats are distributed, depending on the
instrument:

- **Survey file (`.qsf`)** — a complete Qualtrics survey, including any
  skip/display logic. The **HiTOP-HSUM** ships as a `.qsf` because it
  relies on complex display logic. See [Importing a Qualtrics survey
  file](#qualtrics-qsf).
- **Questions file (`.txt`)** — a Qualtrics *Advanced Format* text file
  containing the questions, imported into a survey you create. The
  **PID-5** (Full/SF/BF), **HiTOP-SR**, and **HiTOP-BR** ship as `.txt`.
  See [Importing a Qualtrics questions file](#qualtrics-txt).

REDCap always ships as an instrument `.zip`. See [Importing a REDCap
instrument](#redcap-zip).

## Importing a Qualtrics survey file (.qsf)

A `.qsf` file creates a whole new survey, complete with its display
logic. Do **not** open or edit the `.qsf` file before importing —
editing it can corrupt the contents so that Qualtrics can no longer read
it. You may rename the file if you wish.

**From the Projects page (creates a new project):**

1.  Log in to Qualtrics and go to your **Projects** page.
2.  Click **Create project**, then choose to create a project **from a
    file**.
3.  Select **Import a QSF file** and click **Choose File** to browse for
    the downloaded `.qsf`.
4.  Click **Create project**.

**From within an existing survey (Survey tab):**

1.  Open a survey and go to the **Survey** tab.
2.  Click **Tools**, then **Import/Export**, then **Import survey**.
3.  Click **Choose File** and select the downloaded `.qsf`.
4.  Choose a **Project Category** (Research Core applies to most
    surveys) and click **Import**.

After importing, review the survey and run a quick preview to confirm
the skip/display logic behaves as expected.

## Importing a Qualtrics questions file (.txt)

A `.txt` file uses Qualtrics’ *Advanced Format* to add the instrument’s
questions to a survey. Keep the file exactly as downloaded — it must
stay in plain **tab-delimited text**; re-saving it as UTF-16 will
prevent it from uploading.

1.  Log in to Qualtrics and create (or open) the survey that will hold
    the instrument.
2.  Go to the **Survey** tab, click **Tools**, then **Import/Export**,
    then **Import survey**.
3.  Click **Choose File** and select the downloaded `.txt`.
4.  Click **Import**. The instrument’s questions are added to your
    survey.
5.  Review the imported questions for accuracy and preview the survey.

## Importing a REDCap instrument (.zip)

The REDCap `.zip` is a packaged data collection instrument that you
upload into an existing REDCap project.

1.  Log in to your REDCap account and navigate to the desired project.
2.  Click on the **Designer** link in the left menu bar under “Project
    Home and Design”.
3.  In the main page, under **Data Collection Instruments**, look for
    the **Upload instrument ZIP** option and click the **Upload**
    button.
4.  Click **Choose File**, navigate to where you have the measure saved
    as a ZIP folder, and select the ZIP folder containing your
    instrument.
5.  Click the **Upload instrument ZIP** button to begin the import
    process.
6.  Find the imported instrument in your list of measures and review it
    for accuracy.
7.  Test the instrument to ensure proper functionality within your
    project.

------------------------------------------------------------------------

*Qualtrics import steps follow the official Qualtrics support article,
[Import & Export
Surveys](https://www.qualtrics.com/support/survey-platform/survey-module/survey-tools/import-and-export-surveys/).
Platform interfaces change over time; if a menu name differs, consult
that article for the current wording.*

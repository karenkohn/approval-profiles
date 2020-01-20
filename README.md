# Scripts for Reformatting and Merging Approval Profiles

These scripts read a profile from a csv file, tweak the structure, and merge all the profiles into one table.

The first script is for the subject parameters (i.e. the call numbers). It interprets the hierarchy of call numbers so
that it becomes possible to look at an individual line of the profile and understand where that call number fits in the hierarchy.
The script also allows you to merge all profiles into one file, retaining information about which profile each line refers to.
This allows for viewing the full range of call numbers that are on all profiles as well as finding which ranges are on
multiple profiles.

The second script reads the publisher information from the profiles. It too allows you to merge all profiles into one file,
retaining information about which profile each line refers to.
This facilitates comparisons across profiles and finding inconsistencies or duplications.

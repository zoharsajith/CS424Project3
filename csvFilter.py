
import csv
import gzip
import os
import re
from datetime import datetime

split_on_tabs = re.compile(b'\t+').split


def main():
    os.chdir(os.path.dirname(os.path.abspath(__file__)))
    if not os.path.isdir('../data'):
        os.makedirs('../data')

    # Load movie titles.

    titles = set()
    uninteresting_titles = set()
    genreWant = set()
    genreNah = set()

    lines = iter(gzip.open('genres.list.gz'))
    line = next(lines)
    while line != b'8: THE GENRES LIST\n':
        line = next(lines)
    assert next(lines) == b'==================\n'
    assert next(lines) == b'\n'

    print('Reading "genres.list.gz" to find interesting movies')

    for line in lines:
        if not_a_real_movie(line):
            continue

        fields = split_on_tabs(line.strip(b'\n'))
        raw_title = fields[0]
        genre = fields[1]

        try:
            raw_title.decode('ascii')
        except UnicodeDecodeError:
            continue

        if genre in (b'Short', b'Adult', b'Reality-TV', b'Talk-Show', b'Game-Show', b'News'):
            uninteresting_titles.add(raw_title)
            genreNah.add(genre)
        else:
            titles.add(raw_title)
            genreWant.add(genre)

    interesting_titles = titles - uninteresting_titles
    interestingGeneres = genreWant - genreNah
    del genreNah
    del genreWant
    del titles
    del uninteresting_titles

    print('Found {0} titles'.format(len(interesting_titles)))

    print('Writing "titles.csv"')

    with open('../data/titles.csv', 'w') as f:
        output = csv.writer(f)
        output.writerow(('title', 'year', 'genre'))
        for raw_title in interesting_titles:
            title_and_year = parse_title(raw_title)
            for gen in interestingGeneres:
                output.writerows([title_and_year, gen])
            

    print('Finished writing "titles.csv"')

def not_a_real_movie(line):
    return (
        line.startswith(b'"')       # TV show
        or b'{' in line             # TV episode
        or b' (????' in line        # Unknown year
        or b' (TV)' in line         # TV Movie
        or b' (V)' in line          # Video
        or b' (VG)' in line         # Video game
        or b' (internet)' in line   # Internet
        or b' (re-release)' in line # Re-Release
        or b' (Blu-ray premiere)' in line 
        )

match_title = re.compile(r'^(.*) \((\d+)(/[IVXL]+)?\)$').match


def parse_title(raw_title):
    try:
        title = raw_title.decode('ascii')
    except UnicodeDecodeError:
        return None, None

    m = match_title(title)
    title = m.group(1)
    year = int(m.group(2))
    numeral = m.group(3)

    if numeral is not None:
        numeral = numeral.strip('/')
        if numeral != 'I':
            title = '{0} ({1})'.format(title, numeral)

    return title, year

def swap_names(name):
    if name.endswith(' (I)'):
        name = name[:-4]
    if ',' in name:
        last, first = name.split(',', 1)
        name = first.strip() + ' ' + last.strip()
    return name


def decode_ascii(s):
    return s.decode('ascii', 'replace').replace(u'\ufffd', u'?')


if __name__ == '__main__':
    main()
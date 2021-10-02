#'////////////////////////////////////////////////////////////////////////////
#' FILE: ghReleaseDownloader.py
#' AUTHOR: David Ruvolo
#' CREATED: 2021-09-10
#' MODIFIED: 2021-10-02
#' PURPOSE: Find and download release from a Github repo
#' STATUS: working
#' PACKAGES: os, requests, datatable, datetime, tarfile
#' COMMENTS: see example below
#'
#' ```python
#' import ghReleaseDownloader
#' gh = ghReleaseDownloader(owner = "<github username>", repo = "<repo name>")
#' gh.listReleases()
#' gh.downloadRelease(outDir = 'downloads/', tag_name = 'latest')
#' ```
#'////////////////////////////////////////////////////////////////////////////

import os
import requests
from datatable import dt, f
from datetime import datetime
import tarfile


class ghReleaseDownloader:
    """GitHub Release Download

    View and download a release of a pubically available GitHub repository

    @param owner (str) : the GitHub username
    @param repo  (str) : the repository name

    """
    def __init__(self, owner: str = None, repo: str = None):
        self.releases = []
        self.gh_host = 'https://api.github.com'
        self.gh_owner = owner
        self.gh_repo = repo
        self.gh_endpoint_release = None
        self.gh_default_header = {'Accept': 'application/vnd.github.v3+json'}
        self.__build__release__url__()
        
    def __build__release__url__(self):
        """Build Url
        Build endpoint when owner and repo are supplied
        """
        self.gh_endpoint_release = '{}/repos/{}/{}/releases'.format(
            self.gh_host,
            self.gh_owner,
            self.gh_repo
        )

    def __print__releases(self):
        """Print Release Overview

        Print all available releases 

        """
        print(self.releases[:, ['id', 'name', 'tag_name', 'published_at']])


    def __format__date(self, date):
        """Format Date

        Return date as yyyy-mm-dd

        @param date (date) : datetime object

        @return date
        """
        if not date:
            return None
        return datetime.strptime(str(date), '%Y-%m-%dT%H:%M:%SZ').date()
    

    def listReleases(self, per_page: int = 30, page: int = 1):
        """List Available Releases
        
        List current releases (tagged as an release)

        @param per_page (int) : the number of results per page (default: 30)
        @param page     (int) : the page number of results to fetch (default: 1)

        @reference
        \url{https://docs.github.com/en/rest/reference/repos#releases}
        @return response code or json object
        """

        # if releases dataset has not been built, then fetch information
        if not self.releases:
            headers = self.gh_default_header
            if per_page: headers['per_page'] = str(per_page)
            if page: headers['page'] = str(page)

            try:
                resp = requests.get(url = self.gh_endpoint_release, headers = headers)
                resp.raise_for_status()
            except requests.exceptions.HTTPError as e:
                raise SystemError(e)
                
            data = resp.json()
            releases = []
            for d in data:
                releases.append({
                    'id' : d.get('id', None),
                    'name': d.get('name', None),
                    'tag_name': d.get('tag_name', None),
                    'created_at': self.__format__date(d.get('created_at', None)),
                    'published_at': self.__format__date(d.get('published_at', None)),
                    'tarball_url': d.get('tarball_url', None),
                    'zipball_url': d.get('zipball_url', None)
                })
            print('Found {} releases'.format(len([d['id'] for d in data])))
            self.releases = dt.Frame(releases)
        
        # otherwise print dataset
        self.__print__releases()
        
    
    def downloadRelease(self, outDir: str = '.', tagName: str = "latest"):
        """Download Release

        Download a release by `tag_name`

        @param outDir  (str) : path to download and extract contents
        @param tagName (str) : release tag (default: 'latest')
        
        """
        dir = os.path.abspath(outDir)
        
        release = tagName
        if release == "latest":
            release = self.releases[0, ['tag_name']].to_dict()['tag_name'][0]
        
        url = self.releases[f.tag_name == release, :].to_dict()['tarball_url'][0]
        try:
            print('Downloading Release: {}\nTrying: {}'.format(release, url))
            resp = requests.get(url, headers = self.gh_default_header, stream = True)
            file = tarfile.open(fileobj = resp.raw, mode = 'r|gz')
            file.extractall(path = dir)
            resp.raise_for_status()
            print('Extracted at: {}'.format(dir))
        except requests.exceptions.HTTPError as e:
            raise SystemError(e)

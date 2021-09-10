#'////////////////////////////////////////////////////////////////////////////
#' FILE: ghReleaseDownloader.py
#' AUTHOR: David Ruvolo
#' CREATED: 2021-09-10
#' MODIFIED: 2021-09-10
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
import datatable as dt
from datatable import f
from datetime import datetime
import tarfile

# @name ghReleaseDownloader
# @description Find releases and download them
class ghReleaseDownloader:
    def __init__(self, owner: str = None, repo: str = None):
        self.releases = []
        self.gh_host = 'https://api.github.com'
        self.gh_owner = owner
        self.gh_repo = repo
        self.gh_endpoint_release = None
        self.gh_default_header = {'Accept': 'application/vnd.github.v3+json'}
        self.__build__release__url__()
        
    def __build__release__url__(self):
        self.gh_endpoint_release = '{}/repos/{}/{}/releases'.format(
            self.gh_host,
            self.gh_owner,
            self.gh_repo
        )

    def __print__releases(self):
        print(self.releases[:, ['id', 'name', 'tag_name', 'published_at']])
        
    def __format__date(self, date):
        if not date:
            return None
        return datetime.strptime(str(date), '%Y-%m-%dT%H:%M:%SZ').date()
    
    #' @name listReleases
    #' @description List current HPO releases (tagged as an release)
    #' @param per_page number of results per page (default: 30)
    #' @param page page number of results to fetch (default: 1)
    #' @reference
    #' \url{https://docs.github.com/en/rest/reference/repos#releases}
    #' @return response code or json object
    def listReleases(self, per_page: int = 30, page: int = 1):
        
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
        
    #' @name downloadRelease
    #' @description Download a release by tag_name
    #' @param tag_name release tag name (use listReleases)
    #' @return
    def downloadRelease(self, outDir: str = '.', tag_name: str = "latest"):
        dir = os.path.abspath(outDir)
        
        release = tag_name
        if release == "latest":
            release = self.releases[0, ['tag_name']].to_dict()['tag_name'][0]
        
        url = self.releases[f.tag_name == release, :].to_dict()['tarball_url'][0]
        path = dir + '/' + os.path.basename(url) + '.tar.gz'
        try:
            print('Downloading Release: {}\nTrying: {}'.format(release, url))
            resp = requests.get(url, headers = self.gh_default_header, stream = True)
            file = tarfile.open(fileobj = resp.raw, mode = 'r|gz')
            file.extractall(path = dir)
            resp.raise_for_status()
            print('Extracted at: {}'.format(dir))
        except requests.exceptions.HTTPError as e:
            raise SystemError(e)

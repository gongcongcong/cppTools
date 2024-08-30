#include <R.h>
#include <Rinternals.h>
#include <unistd.h>
#include <stdlib.h>
#include <dirent.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>

#define R_NO_REMAP

// Function to delete a file or directory recursively
int delete_recursively(const char *path, int *file_count, int limit, int interval, int verbose, int *round) {
        struct stat statbuf;
        int ret = 0;

        if (stat(path, &statbuf) != 0) {
                if (verbose) {
                        Rprintf("Unable to access path: %s\n", path);
                }
                return ret; // Unable to access path, skip it
        }

        if (S_ISDIR(statbuf.st_mode)) {
                DIR *d = opendir(path);
                if (!d) {
                        if (verbose) {
                                Rprintf("Unable to open directory: %s\n", path);
                        }
                        return ret;
                }

                struct dirent *entry;
                while ((entry = readdir(d)) != NULL) {
                        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
                                continue; // Skip . and ..
                        }
                        char full_path[1024];
                        snprintf(full_path, sizeof(full_path), "%s/%s", path, entry->d_name);
                        ret += delete_recursively(full_path, file_count, limit, interval, verbose, round);
                }
                closedir(d);
                rmdir(path); // Remove the directory after its contents are deleted
                if (verbose) {
                        Rprintf("Deleted directory: %s\n", path);
                }
        } else {
                unlink(path); // Remove the file
                (*file_count)++;
                ret++;

                if (*file_count >= limit) {
                        Rprintf("Round %d\n\t", *round);
                        Rprintf("\rDeleting... %d files deleted\n", ret);
                        (*round)++;
                        sleep(interval); // Pause for the interval
                        *file_count = 0; // Reset the count after a batch
                }
        }
        return ret;
}

SEXP R_delete_files(SEXP folder_path, SEXP num_to_delete, SEXP interval_sec, SEXP show_more) {
        // Check types of inputs to ensure they are correct
        if (!Rf_isString(folder_path) || LENGTH(folder_path) != 1) {
                Rf_error("folder_path must be a single string");
        }
        if (!Rf_isInteger(num_to_delete) || LENGTH(num_to_delete) != 1) {
                Rf_error("num_to_delete must be a single integer");
        }
        if (!Rf_isInteger(interval_sec) || LENGTH(interval_sec) != 1) {
                Rf_error("interval_sec must be a single integer");
        }
        if (!Rf_isLogical(show_more) || LENGTH(show_more) != 1) {
                Rf_error("show_more must be a single locgical");
        }

        const char *path = CHAR(STRING_ELT(folder_path, 0));
        int num_delete = INTEGER(num_to_delete)[0];
        int interval = INTEGER(interval_sec)[0];
        int verbose = LOGICAL(show_more)[0];

        DIR *d = opendir(path);
        if (!d) {
                Rprintf("Unable to open directory: %s\n", path);
                return R_NilValue;
        }

        struct dirent *entry;
        int total_deleted = 0;
        int file_count = 0;
        int round = 1;

        while ((entry = readdir(d)) != NULL) {
                if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
                        continue; // Skip . and ..
                }

                char full_path[1024];
                snprintf(full_path, sizeof(full_path), "%s/%s", path, entry->d_name);
                total_deleted += delete_recursively(full_path, &file_count, num_delete, interval, verbose, &round);
        }

        closedir(d);
        Rprintf("\nDeletion complete: %d files deleted.\n", total_deleted);
        return R_NilValue;
}


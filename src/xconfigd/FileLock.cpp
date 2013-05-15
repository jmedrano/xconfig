#include "FileLock.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <unistd.h>

#include <QFile>

FileLock::FileLock(const QString& _name, bool locked) : name(QFile::encodeName(_name)), fd(-1)
{
	if (locked)
		lock();
}

FileLock::~FileLock()
{
	unlock();
}

void FileLock::lock()
{
	do_lock(true);
}

void FileLock::unlock()
{
	struct stat f_fd, f_name;

	if (fd == -1)
		return;

	// There's a race condition here, but...
	if (stat(name.constData(), &f_name) != 0 || fstat(fd, &f_fd) != 0)
		goto get_out;
	if ((f_fd.st_dev == f_name.st_dev) && (f_fd.st_ino == f_name.st_ino))
		unlink(name.constData());

get_out:
	flock(fd, LOCK_UN);
	close(fd);
	fd = -1;
}

bool FileLock::tryLock()
{
	return do_lock(false);
}

bool FileLock::do_lock(bool block)
{
	bool created = true;

	if (fd != -1)
		return true;

	fd = open(name.constData(), O_WRONLY | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
	if (fd == -1) {
		created = false;
		fd = open(name.constData(), O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR);
	}

	if (fd == -1)
		return false;

	if (block) {
		int r;

		do {
			r = flock(fd, LOCK_EX);
		} while (r != 0 && errno == EINTR);

		if (r != 0)
			goto error;
	} else {
		if (flock(fd, LOCK_EX | LOCK_NB) != 0)
			goto error;
	}

	return true;

error:
	if (created)
		unlink(name.constData());
	close(fd);
	fd = -1;
	return false;
}


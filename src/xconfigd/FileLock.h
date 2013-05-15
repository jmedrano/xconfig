#ifndef FILELOCK_H
#define FILELOCK_H

#include <QString>

class FileLock {
public:
	FileLock(const QString& name, bool locked = false);
	~FileLock();

	void lock();
	void unlock();
	bool tryLock();

private:
	bool do_lock(bool block);

private:
	QByteArray name;
	int fd;
};

#endif // FILELOCK_H

#include "ConnectionManager.h"

#include <QIODevice>

#define DECLARE_CAP(c) c, sizeof(c) - 1
static const int VERSION = 1;
static struct { const char *name; size_t len; ConnectionManager::Capability capability; } CAPABILITIES[] = {
	{ DECLARE_CAP("FD_PASS"), ConnectionManager::CAP_FD_PASS }
};
static const int MAX_LINE = 1024;
static const char* HANDSHAKE_EXPECTED = "AKITOLOKO CAPABILITIES (";
static const int HANDSHAKE_EXPECTED_LEN = strlen(HANDSHAKE_EXPECTED);

ConnectionManager::ConnectionManager(QIODevice* connection, QObject* parent) : QObject(parent), connection(connection), state(STATE_INVALID), buffer_pos(0)
{
	QByteArray handshake;

	handshake.append("OLAKEASE XConfigD VERSION ");
	handshake.append(QString::number(VERSION));
	handshake.append(" CAPABILITIES (");
	for (unsigned int i = 0; i < (sizeof(CAPABILITIES) / sizeof(CAPABILITIES[0])); i++) {
		handshake.append(CAPABILITIES[i].name);
		if (i != (sizeof(CAPABILITIES) / sizeof(CAPABILITIES[0])) - 1)
			handshake.append(",");
	}
	handshake.append(")\n");
	connection->write(handshake);
	state = STATE_RECEIVING_HANDSHAKE;

	connect(connection, SIGNAL(readyRead()), SLOT(connection_ready_read()));
	connect(connection, SIGNAL(error(QLocalSocket::LocalSocketError)), SLOT(connection_error(QLocalSocket::LocalSocketError)));
	buffer.resize(MAX_LINE);
	connection->setParent(this);
}

ConnectionManager::~ConnectionManager()
{
	delete connection;
}

void ConnectionManager::connection_ready_read()
{
	int eol, start_line = 0, search_pos = buffer_pos;

//	printf ("ready_read\n");
	qint64 size_read = connection->read(buffer.data() + buffer_pos, buffer.capacity() - buffer_pos);
	buffer_pos += size_read;
//	printf("buffer size = %d (cap = %d)\n", buffer_pos, buffer.capacity());

	while ((eol = buffer.indexOf('\n', search_pos)) != -1 && eol < buffer_pos) {
//		printf("loop %d %d\n", search_pos, eol);
		buffer[eol] = '\0';
		if (state == STATE_RECEIVING_HANDSHAKE) {
			if (!got_handshake(buffer.data() + start_line, eol - start_line)) {
				abort();
				return;
			}
			state = STATE_RECEIVING_LINE;
		} else if (state == STATE_RECEIVING_LINE) {
			if (!got_line(buffer.data() + start_line, eol - start_line)) {
				abort();
				return;
			}
		}
		start_line = search_pos = eol + 1;
	}
	if (start_line > 1)
		memmove(buffer.data(), buffer.data() + start_line, start_line - 1);
	buffer_pos -= start_line;
	if (buffer_pos == buffer.capacity())
		abort();
}

void ConnectionManager::connection_error(QLocalSocket::LocalSocketError error)
{
	Q_UNUSED(error);
	printf("error\n");
	abort();
}

bool ConnectionManager::got_handshake(const char *handshake, size_t len)
{
	printf("got handshake: '%s'\n", handshake);
	if (strncmp(handshake, HANDSHAKE_EXPECTED, HANDSHAKE_EXPECTED_LEN) != 0)
		return false;
	const char *haystack = handshake + strlen(HANDSHAKE_EXPECTED);
	const char *needle;
//	printf("%s\n", haystack);
	while ((needle = strpbrk(haystack, ",)")) != NULL) {
		bool found = false;
		if ((needle - haystack) == 0 && *needle == ')')
			break;
		for (unsigned int i = 0; i < (sizeof(CAPABILITIES) / sizeof(CAPABILITIES[0])); i++) {
//			printf("%d %d\n", needle-haystack, CAPABILITIES[i].len);
			if ((size_t)(needle - haystack) == CAPABILITIES[i].len && strncmp(CAPABILITIES[i].name, haystack, qMin((size_t)(needle - haystack), strlen(CAPABILITIES[i].name))) == 0) {
				if (capabilities & CAPABILITIES[i].capability)
					return false;
				capabilities |= CAPABILITIES[i].capability;
				found = true;
//				printf("cap found\n");
				break;
			}
		}
		if (!found)
			return false;
		if (*needle == ')')
			break;
		haystack = needle + 1;
	}

//	printf("%d => %d\n", len, (size_t)(needle - handshake + 1));
	if (!needle || *needle != ')' || (len != (size_t)(needle - handshake + 1)))
		return false;
	return true;
}

bool ConnectionManager::got_line(const char *line, size_t len)
{
	printf("got line: '%s'\n", line);
	return true;
}

void ConnectionManager::abort()
{
	printf("abort\n");
	deleteLater();
}


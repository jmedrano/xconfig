#ifndef TLOGGER_H
#define TLOGGER_H

#include <QMetaObject>

#include <log4cxx/logger.h>
#include <log4cxx/basicconfigurator.h>
#include <log4cxx/xml/domconfigurator.h>

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#include <stdio.h>
#undef _GNU_SOURCE
#else
#include <stdio.h>
#endif
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#define _ARG16(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, ...) _15
#define HAS_COMMA(...) _ARG16(__VA_ARGS__, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
#define _TRIGGER_PARENTHESIS_(...) ,
#define ISEMPTY(...) _ISEMPTY(HAS_COMMA(__VA_ARGS__), HAS_COMMA(_TRIGGER_PARENTHESIS_ __VA_ARGS__), HAS_COMMA(__VA_ARGS__ (/*empty*/)), HAS_COMMA(_TRIGGER_PARENTHESIS_ __VA_ARGS__ (/*empty*/)))
#define PASTE5(_0, _1, _2, _3, _4) _0 ## _1 ## _2 ## _3 ## _4
#define _ISEMPTY(_0, _1, _2, _3) HAS_COMMA(PASTE5(_IS_EMPTY_CASE_, _0, _1, _2, _3))
#define _IS_EMPTY_CASE_0001 ,

#define T_LOGGER_VAR _logger

#define T_LOGGER_DECLARE(klass) static log4cxx::LoggerPtr T_LOGGER_VAR
#define T_QLOGGER_DEFINE(klass) log4cxx::LoggerPtr klass::T_LOGGER_VAR(log4cxx::Logger::getLogger(TLogger::HierarchyNamer(&klass::staticMetaObject).name()))
#define T_QLOGGER_DEFINE_OTHER(klass, other) log4cxx::LoggerPtr klass::T_LOGGER_VAR(log4cxx::Logger::getLogger(TLogger::HierarchyNamer(&other::staticMetaObject).name()))
#define T_LOGGER_DEFINE(klass, name) log4cxx::LoggerPtr klass::T_LOGGER_VAR(log4cxx::Logger::getLogger(name))
#define T_QLOGGER_DEFINE_ROOT(klass) log4cxx::LoggerPtr klass::T_LOGGER_VAR(log4cxx::Logger::getLogger(TLogger::HierarchyNamer(&klass::staticMetaObject, TLoggerRoot()).name()))
#define T_QLOGGER_DEFINE_OTHER_ROOT(klass, other) log4cxx::LoggerPtr klass::T_LOGGER_VAR(log4cxx::Logger::getLogger(TLogger::HierarchyNamer(&other::staticMetaObject, TLoggerRoot()).name()))
#define T_LOGGER_DEFINE_ROOT(klass, name) log4cxx::LoggerPtr klass::T_LOGGER_VAR(log4cxx::Logger::getLogger(TLogger::MessageFormatter("%s.%s", TLoggerRoot(), name)))

#ifdef NDEBUG
#define TTRACE(message_, ...) do { } while(0);
#else
#define TTRACE(message_, ...) do { if (ISEMPTY(__VA_ARGS__)) { LOG4CXX_TRACE(T_LOGGER_VAR, (message_)); } else { LOG4CXX_TRACE(T_LOGGER_VAR, TLogger::MessageFormatter(message_, ##__VA_ARGS__).message()); } } while(0);
#endif

#define TDEBUG(message_, ...) do {if (ISEMPTY(__VA_ARGS__)) { LOG4CXX_DEBUG(T_LOGGER_VAR, (message_)); } else { LOG4CXX_DEBUG(T_LOGGER_VAR, TLogger::MessageFormatter(message_, ##__VA_ARGS__).message()); } } while(0);
#define TINFO(message_, ...) do { if (ISEMPTY(__VA_ARGS__)) { LOG4CXX_INFO(T_LOGGER_VAR, (message_)); } else { LOG4CXX_INFO(T_LOGGER_VAR, TLogger::MessageFormatter(message_, ##__VA_ARGS__).message()); } } while(0);
#define TWARN(message_, ...) do {if (ISEMPTY(__VA_ARGS__)) { LOG4CXX_WARN(T_LOGGER_VAR, (message_)); } else { LOG4CXX_WARN(T_LOGGER_VAR, TLogger::MessageFormatter(message_, ##__VA_ARGS__).message()); } } while(0);
#define TERROR(message_, ...) do {if (ISEMPTY(__VA_ARGS__)) { LOG4CXX_ERROR(T_LOGGER_VAR, (message_)); } else { LOG4CXX_ERROR(T_LOGGER_VAR, TLogger::MessageFormatter(message_, ##__VA_ARGS__).message()); } } while(0);
#define TFATAL(message_, ...) do { if (ISEMPTY(__VA_ARGS__)) { LOG4CXX_FATAL(T_LOGGER_VAR, (message_)); } else { LOG4CXX_FATAL(T_LOGGER_VAR, TLogger::MessageFormatter(message_, ##__VA_ARGS__).message()); } } while(0);

namespace TLogger {
class MessageFormatter {
public:
	explicit MessageFormatter(const char* format, ...) __attribute__((format(printf, 2, 3)));
	~MessageFormatter() { free(message_); }

	const char* message() const { return message_; }

private:
	char* message_;
};

inline MessageFormatter::MessageFormatter(const char* format, ...)
{
	va_list args;
	va_start(args, format);
	if (vasprintf(&message_, format, args) == -1)
		throw std::bad_alloc();
	va_end(args);
}

class HierarchyNamer {
public:
	explicit HierarchyNamer(const QMetaObject* mo, const char* root = 0);
	~HierarchyNamer() { free(name_); }

	const char* name() const { return name_; }

private:
	void calc_hierarchy_name(const QMetaObject* mo);
	char* name_;
};

inline HierarchyNamer::HierarchyNamer(const QMetaObject* mo, const char* root) : name_(0)
{
	calc_hierarchy_name(mo);
	if (root) {
		char* name = 0;
		if (asprintf(&name, "%s.%s", root, name_) == -1)
			throw std::bad_alloc();
		free(name_);
		name_ = name;
	}
}

inline void HierarchyNamer::calc_hierarchy_name(const QMetaObject* mo)
{
	if (!mo || *mo->className() == 'Q')
		return;

	calc_hierarchy_name(mo->superClass());

	if (name_) {
		char* name = 0;
		if (asprintf(&name, "%s.%s", name_, mo->className()) == -1)
			throw std::bad_alloc();
		free(name_);
		name_ = name;
	} else {
		name_ = strdup(mo->className());
	}
}

inline void configure(const char* filename = 0)
{
	log4cxx::BasicConfigurator::resetConfiguration();
	if (filename)
		log4cxx::xml::DOMConfigurator::configure(filename);
	else
		log4cxx::BasicConfigurator::configure();
}

} // namespace TLogger

extern const char* TLoggerRoot();

#endif // TLOGGER_H

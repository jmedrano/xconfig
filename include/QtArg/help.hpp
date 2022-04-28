
/*!
	\file

	\brief Help printer.

	\author Igor Mironchik (igor.mironchik at gmail dot com).

	Copyright (c) 2010-2012 Igor Mironchik

	Permission is hereby granted, free of charge, to any person
	obtaining a copy of this software and associated documentation
	files (the "Software"), to deal in the Software without
	restriction, including without limitation the rights to use,
	copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the
	Software is furnished to do so, subject to the following
	conditions:

	The above copyright notice and this permission notice shall be
	included in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
	OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
	NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
	HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
	WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
	FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
	OTHER DEALINGS IN THE SOFTWARE.
*/

#ifndef QTARG__HELP_HPP__INCLUDED
#define QTARG__HELP_HPP__INCLUDED

// Qt include.
#include <QtGlobal>
#include <QtCore/QTextStream>
#include <QtCore/QMap>

// QtArg include.
#include "cmdline.hpp"
#include "arg.hpp"
#include "helpiface.hpp"
#include "exceptions.hpp"
#include "xorarg.hpp"
#include "multiarg.hpp"

#if QT_VERSION >= QT_VERSION_CHECK(5, 14, 0)
#define QT_ENDL Qt::endl
#else
#define QT_ENDL endl
#endif

//
// QtArgDefaultHelpPrinter
//

//! Default implementation of the QtArgHelpPrinterIface.
class QtArgDefaultHelpPrinter
	:	public QtArgHelpPrinterIface
{
	friend class QtArgHelp;

	private:
		QtArgDefaultHelpPrinter( const QtArgDefaultHelpPrinter & );
		QtArgDefaultHelpPrinter & operator = ( const QtArgDefaultHelpPrinter & );

	protected:
		QtArgDefaultHelpPrinter() {}
		virtual ~QtArgDefaultHelpPrinter() {}

	public:
		//! \return Static instance of this help printer.
		static QtArgDefaultHelpPrinter * instance()
		{
			static QtArgDefaultHelpPrinter printer;

			return &printer;
		}

		//! Print help.
		virtual void print() const;

		/*!
			Set program description that will be used
			as first line in the help.
		*/
		virtual void setProgramDescription( const QString & desc );

		/*!
			Set executable name that defined in argv[0]
			or something else.
		*/
		virtual void setExecutableName( const QString & exec );

		/*!
			Set "USAGE" string.
		*/
		virtual void setUsageString( const QString & usage );

		/*!
			Set "HELP" string.
		*/
		virtual void setHelpString( const QString & help );

		/*!
			Set delimiter character.
		*/
		virtual void setDelimiterChar( const QChar & delim );

	private:
		//! Replace markers in "USAGE" string.
		void replaceMarkersInUsage();
		//! Replace markers in "HELP" string.
		void replaceMarkersInHelp();
		//! \return Max length of the names in the help.
		int calcMaxNamesLength();
		//! \return Max length of the flags in the help.
		int calcMaxFlagsLength();

	private:
		//! "HELP" string.
		QString m_help;
		//! "USAGE" string.
		QString m_usage;

		//! Program description printed before help.
		QString m_programDescription;
		//! Executable name.
		QString m_executableName;

		//! Flags marker string.
		QString m_flagsMarker;
		//! Names marker string.
		QString m_namesMarker;

		//! Value of the QtArgHelpPrinterIface::argValue.
		static const QString m_argValue;
		//! Value of the QtArgHelpPrinterIface::namesSeparator.
		static const QString m_namesSeparator;
}; // class QtArgDefaultHelpPrinter


//
// QtArgDefaultHelpPrinter implementation.
//

const QString QtArgDefaultHelpPrinter::m_argValue =
	QLatin1String( " <arg>" );

const QString QtArgDefaultHelpPrinter::m_namesSeparator =
	QLatin1String( ", " );

inline void
QtArgDefaultHelpPrinter::print() const
{
	QTextStream stream( stdout );

	stream << m_programDescription << QT_ENDL << QT_ENDL;
	stream << QLatin1String( "USAGE: " ) << m_executableName
		<< ' ' << m_usage << QT_ENDL << QT_ENDL;
	stream << m_help;
}

inline void
QtArgDefaultHelpPrinter::replaceMarkersInUsage()
{
	m_usage.remove( QtArgHelpPrinterIface::beforeFlags );
	m_usage.remove( QtArgHelpPrinterIface::beforeNames );
	m_usage.remove( QtArgHelpPrinterIface::beforeDescription );
	m_usage.remove( QtArgHelpPrinterIface::newLine );

	m_usage.replace( QtArgHelpPrinterIface::afterFlags,
		QLatin1String( "," ) );
	m_usage.replace( QtArgHelpPrinterIface::namesSeparator,
		QLatin1String( "," ) );
	m_usage.replace( QtArgHelpPrinterIface::flagMarker,
		m_flagsMarker );
	m_usage.replace( QtArgHelpPrinterIface::nameMarker,
		m_namesMarker );
	m_usage.replace( QtArgHelpPrinterIface::argValue, m_argValue );
}

inline int
QtArgDefaultHelpPrinter::calcMaxFlagsLength()
{
	int maxLength = 0;
	int pos = 0;

	while( true )
	{
		const int beforeFlagsIdx =
			m_help.indexOf( QtArgHelpPrinterIface::beforeFlags, pos );

		if( beforeFlagsIdx != -1 )
		{
			const int afterFlagsIdx =
				m_help.indexOf( QtArgHelpPrinterIface::afterFlags,
					beforeFlagsIdx );

			const int newLineIdx =
				m_help.indexOf( QtArgHelpPrinterIface::newLine,
					beforeFlagsIdx );

			int length = 0;

			if( afterFlagsIdx != -1 && newLineIdx > afterFlagsIdx )
			{
				length = afterFlagsIdx - beforeFlagsIdx -
					QtArgHelpPrinterIface::beforeFlags.length();

				pos = afterFlagsIdx;
			}
			else
			{
				const int beforeDescriptionIdx =
					m_help.indexOf( QtArgHelpPrinterIface::beforeDescription,
						beforeFlagsIdx );

				length = beforeDescriptionIdx - beforeFlagsIdx -
					QtArgHelpPrinterIface::beforeFlags.length();

				pos = beforeDescriptionIdx;
			}

			if( length > maxLength )
				maxLength = length;
		}
		else
			break;
	}

	return maxLength;
}

inline int
QtArgDefaultHelpPrinter::calcMaxNamesLength()
{
	int maxLength = 0;
	int pos = 0;

	while( true )
	{
		int delta = 0;
		const int afterFlagsIdx = m_help.indexOf(
			QtArgHelpPrinterIface::afterFlags, pos );
		const int beforeNamesIdx = m_help.indexOf(
			QtArgHelpPrinterIface::beforeNames, pos );

		int startPos = 0;

		if( afterFlagsIdx != -1 && beforeNamesIdx != -1 )
		{
			if( afterFlagsIdx < beforeNamesIdx )
			{
				startPos = afterFlagsIdx;
				delta = QtArgHelpPrinterIface::afterFlags.length();
			}
			else
			{
				startPos = beforeNamesIdx;
				delta = QtArgHelpPrinterIface::beforeNames.length();
			}
		}
		else if( beforeNamesIdx != -1 )
		{
			startPos = beforeNamesIdx;
			delta = QtArgHelpPrinterIface::beforeNames.length();
		}
		else if( afterFlagsIdx != -1 )
		{
			startPos = afterFlagsIdx;
			delta = QtArgHelpPrinterIface::afterFlags.length();
		}
		else
			break;

		const int endPos =
			m_help.indexOf( QtArgHelpPrinterIface::beforeDescription,
				startPos );

		const int length = endPos - startPos - delta;

		if( length > maxLength )
			maxLength = length;

		pos = endPos;
	}

	return maxLength;
}

inline void
QtArgDefaultHelpPrinter::replaceMarkersInHelp()
{
	m_help.replace( QtArgHelpPrinterIface::argValue, m_argValue );
	m_help.replace( QtArgHelpPrinterIface::flagMarker, m_flagsMarker );
	m_help.replace( QtArgHelpPrinterIface::nameMarker, m_namesMarker );
	m_help.replace( QtArgHelpPrinterIface::namesSeparator, m_namesSeparator );

	const int beforeFlagsLength = 2;
	const int afterNamesLength = 3;

	const int beforeNamesLength = beforeFlagsLength +
		calcMaxFlagsLength() + 2;
	const int beforeDescLength = beforeNamesLength +
		calcMaxNamesLength() + afterNamesLength;

	while( true )
	{
		const int beforeFlagsIdx =
			m_help.indexOf( QtArgHelpPrinterIface::beforeFlags );
		int afterFlagsIdx =
			m_help.indexOf( QtArgHelpPrinterIface::afterFlags );
		const int beforeNamesIdx =
			m_help.indexOf( QtArgHelpPrinterIface::beforeNames );
		int beforeDescIdx =
			m_help.indexOf( QtArgHelpPrinterIface::beforeDescription );
		int newLineIdx =
			m_help.indexOf( QtArgHelpPrinterIface::newLine );

		int flagsLength = 0;

		if( beforeFlagsIdx != -1 && beforeFlagsIdx < newLineIdx )
		{
			if( afterFlagsIdx != -1 && afterFlagsIdx < newLineIdx )
				flagsLength = afterFlagsIdx - beforeFlagsIdx -
					QtArgHelpPrinterIface::beforeFlags.length();
			else
				flagsLength = beforeDescIdx - beforeFlagsIdx -
					QtArgHelpPrinterIface::beforeFlags.length();
		}

		int namesLength = 0;

		if( afterFlagsIdx != -1 && afterFlagsIdx < newLineIdx )
			namesLength = beforeDescIdx - afterFlagsIdx -
				QtArgHelpPrinterIface::afterFlags.length();
		else if( beforeNamesIdx != -1 && beforeNamesIdx < newLineIdx )
			namesLength = beforeDescIdx - beforeNamesIdx -
				QtArgHelpPrinterIface::beforeNames.length();

		int beforeDescSpace = beforeDescLength;

		if( newLineIdx != -1 )
		{
			if( beforeFlagsIdx != -1 && beforeFlagsIdx < newLineIdx )
			{
				m_help.replace( beforeFlagsIdx,
					QtArgHelpPrinterIface::beforeFlags.length(),
					QString( beforeFlagsLength, ' ' ) );

				beforeDescSpace -= ( beforeFlagsLength + flagsLength );

				if( afterFlagsIdx != -1 && afterFlagsIdx < newLineIdx )
				{
					afterFlagsIdx =
						m_help.indexOf( QtArgHelpPrinterIface::afterFlags );

					const int afterFlagsSpace = beforeNamesLength - 1 -
						flagsLength - beforeFlagsLength;

					beforeDescSpace -= ( afterFlagsSpace + 1 + namesLength );

					m_help.replace( afterFlagsIdx,
						QtArgHelpPrinterIface::afterFlags.length(),
						',' + QString( afterFlagsSpace, ' ' ) );
				}
			}
			else if( beforeNamesIdx != -1 && beforeNamesIdx < newLineIdx )
			{
				beforeDescSpace -= ( beforeNamesLength + namesLength );

				m_help.replace( beforeNamesIdx,
					QtArgHelpPrinterIface::beforeNames.length(),
					QString( beforeNamesLength, ' ' ) );
			}

			beforeDescIdx =
				m_help.indexOf( QtArgHelpPrinterIface::beforeDescription );

			m_help.replace( beforeDescIdx,
				QtArgHelpPrinterIface::beforeDescription.length(),
				QString( beforeDescSpace, ' ' ) );

			newLineIdx = m_help.indexOf( QtArgHelpPrinterIface::newLine );

			m_help.replace( newLineIdx,
				QtArgHelpPrinterIface::newLine.length(),
				QLatin1String( "\n" ) );
		}
		else
			break;
	}
}

inline void
QtArgDefaultHelpPrinter::setProgramDescription( const QString & desc )
{
	m_programDescription = desc;
}

inline void
QtArgDefaultHelpPrinter::setExecutableName( const QString & exec )
{
	m_executableName = exec;
}

inline void
QtArgDefaultHelpPrinter::setUsageString( const QString & usage )
{
	m_usage = usage;

	replaceMarkersInUsage();
}

inline void
QtArgDefaultHelpPrinter::setHelpString( const QString & help )
{
	m_help = help;

	replaceMarkersInHelp();
}

inline void
QtArgDefaultHelpPrinter::setDelimiterChar( const QChar & delim )
{
	m_flagsMarker = QString( delim );
	m_namesMarker = QString( 2, delim );
}


//
// QtArgHelp
//

//! Argument for the help priniting.
/*!
	Defined as argument with "h" flag and
	"help" name.
*/
class QtArgHelp
	:	public QtArg
{
	public:
		explicit QtArgHelp( QtArgCmdLine * cmdLine );

		virtual ~QtArgHelp();

		//! \return Printer implementation.
		QtArgHelpPrinterIface * printer();

		//! Set printer implementation.
		void setPrinter( QtArgHelpPrinterIface * p );

		//! \name QtArgIface implementation.
		//! \{

			//! Process with visitor.
			virtual void visit( QtArgCmdLineContext & context );

		//! \}

	private:

		//! Create "USAGE" string.
		QString createUsageString();
		//! Create "HELP" string.
		QString createHelpString();
		/*!
			Create "USAGE" string for arguments.

			Result will be appended to \par usage string.

			\par usage Result "USAGE" string.
			\par args List of arguments to process.
			\par before Character to insert before argument
				in "USAGE" string.
			\par after Character to insert after argument
				in "USAGE" string.
		*/
		void createUsageStringForArgs( QString & usage,
			const QtArgCmdLineIface::QtArgumentsList & args,
			const QChar & before, const QChar & after );
		//! Init names list.
		void initNames( const QtArgCmdLineIface::QtArgumentsList & list );
		//! Remove markers from usage string.
		void removeMarkersFromUsageString( QString & usage );
		//! Print help on given argument.
		void printHelp( QtArgIface * arg );
		//! Print help on given argument.
		void printHelp( const QString & argName );

	private:
		//! Printer implemenation.
		QtArgHelpPrinterIface * m_printer;
		//! Pointer to the QtArgCmdLine.
		QtArgCmdLine * m_cmdLine;
		//! Names of all available arguments.
		QtArgIface::NamesList m_names;
}; // class QtArgHelp


//
// QtArgHelp implementation.
//

inline
QtArgHelp::QtArgHelp( QtArgCmdLine * cmdLine )
	:	QtArg( 'h', QLatin1String( "help" ),
			QLatin1String( "Print this help." ) )
	,	m_printer( QtArgDefaultHelpPrinter::instance() )
	,	m_cmdLine( cmdLine )
{
}

inline
QtArgHelp::~QtArgHelp()
{
}

inline QtArgHelpPrinterIface *
QtArgHelp::printer()
{
	return m_printer;
}

inline void
QtArgHelp::setPrinter( QtArgHelpPrinterIface * p )
{
	m_printer = p;
}

inline QString
QtArgHelp::createUsageString()
{
	QtArgCmdLineIface::QtArgumentsList requiredArgs;
	QtArgCmdLineIface::QtArgumentsList optionalArgs;

	foreach( QtArgIface * arg, m_cmdLine->arguments() )
	{
		if( arg->isRequired() )
			requiredArgs.push_back( arg );
		else
			optionalArgs.push_back( arg );
	}

	QString usage;

	createUsageStringForArgs( usage, requiredArgs, QChar::Null, QChar::Null );
	createUsageStringForArgs( usage, optionalArgs, '[', ']' );

	return usage;
}

inline void
QtArgHelp::createUsageStringForArgs( QString & usage,
	const QtArgCmdLineIface::QtArgumentsList & args,
	const QChar & before, const QChar & after )
{
	int count = 1;

	if( usage.length() )
		usage.append( ' ' );

	foreach( QtArgIface * arg, args )
	{
		if( !before.isNull() )
			usage.append( before );

		usage.append( arg->getUsageString( m_names ) );

		if( !after.isNull() )
			usage.append( after );

		if( count < args.size() )
			usage.append( ' ' );
	}
}

inline QString
QtArgHelp::createHelpString()
{
	QtArgCmdLineIface::QtArgumentsList requiredArgs;
	QtArgCmdLineIface::QtArgumentsList optionalArgs;

	foreach( QtArgIface * arg, m_cmdLine->arguments() )
	{
		if( arg->isRequired() )
			requiredArgs.push_back( arg );
		else
			optionalArgs.push_back( arg );
	}

	QString help;

	foreach( QtArgIface * arg, requiredArgs )
		help.append( arg->getHelpString( m_names ) );

	foreach( QtArgIface * arg, optionalArgs )
		help.append( arg->getHelpString( m_names ) );

	return help;
}

inline void
QtArgHelp::removeMarkersFromUsageString( QString & usage )
{
	usage.remove( QtArgHelpPrinterIface::beforeFlags );
	usage.remove( QtArgHelpPrinterIface::beforeNames );
	usage.remove( QtArgHelpPrinterIface::beforeDescription );
	usage.remove( QtArgHelpPrinterIface::newLine );

	usage.replace( QtArgHelpPrinterIface::afterFlags,
		QLatin1String( ", " ) );
	usage.replace( QtArgHelpPrinterIface::namesSeparator,
		QLatin1String( ", " ) );
	usage.replace( QtArgHelpPrinterIface::flagMarker,
		m_cmdLine->delimiter() );
	usage.replace( QtArgHelpPrinterIface::nameMarker,
		QString( 2, m_cmdLine->delimiter() ) );
	usage.replace( QtArgHelpPrinterIface::argValue,
		QLatin1String( " <arg>" ) );
}

inline void
QtArgHelp::printHelp( QtArgIface * arg )
{
	QTextStream out( stdout );

	QString usage = arg->getUsageString( m_names );
	removeMarkersFromUsageString( usage );

	out << QT_ENDL << QLatin1String( "USAGE: " ) << usage << QT_ENDL << QT_ENDL
		<< QLatin1String( "DESCRIPTION: " );

	if( arg->longDescription().length() )
		out << arg->longDescription();
	else
		out << arg->description();

	out << QT_ENDL;

	throw QtArgHelpHasPrintedEx();
}

inline void
QtArgHelp::printHelp( const QString & argName )
{
	if( argName.length() )
	{
		try {
			printHelp( m_cmdLine->findArgument( argName ) );
		}
		catch( const QtArgUnknownArgumentEx & x )
		{
			QTextStream out( stdout );

			out << QString::fromLatin1( "Unknown argument: %1%1%2" )
				.arg( m_cmdLine->delimiter() )
				.arg( argName );

			throw QtArgHelpHasPrintedEx();
		}
	}
	else
	{
		QTextStream out( stdout );

		out << QLatin1String( "Empty argument name." );

		throw QtArgHelpHasPrintedEx();
	}
}

inline void
QtArgHelp::visit( QtArgCmdLineContext & context )
{
	initNames( m_cmdLine->arguments() );

	if( !context.atEnd() )
	{
		QString argName = context.next();

		if( m_cmdLine->isArgument( argName ) )
		{
			argName.replace( 0, 2, QLatin1String( "" ) );

			printHelp( argName );
		}
		else if( m_cmdLine->isFlag( argName ) )
		{
			QTextStream out( stdout );
			argName.replace( 0, 1, QLatin1String( "" ) );

			if( argName.length() )
			{
				try {
					QtArgIface * arg = m_cmdLine->findArgument( argName[ 0 ] );

					foreach( QChar f, argName )
					{
						if( arg != m_cmdLine->findArgument( f ) )
							throw QtArgUnknownArgumentEx( QString() );
					}

					printHelp( arg );
				}
				catch( const QtArgUnknownArgumentEx & x )
				{
					out << QString::fromLatin1( "Unknown argument: %1%2" )
						.arg( m_cmdLine->delimiter() )
						.arg( argName );

					throw QtArgHelpHasPrintedEx();
				}
			}
			else
			{
				out << QLatin1String( "Empty flag." );

				throw QtArgHelpHasPrintedEx();
			}
		}
		else
			printHelp( argName );
	}
	else
	{
		m_printer->setDelimiterChar( m_cmdLine->delimiter() );
		m_printer->setUsageString( createUsageString() );
		m_printer->setHelpString( createHelpString() );

		m_printer->print();
	}

	throw QtArgHelpHasPrintedEx();
}

inline void
QtArgHelp::initNames( const QtArgCmdLineIface::QtArgumentsList & list )
{
	foreach( QtArgIface * arg, list )
	{
		if( arg->arguments().size() > 1 )
			initNames( arg->arguments() );
		else
			m_names.append( arg->names() );
	}
}

#endif // QTARG__HELP_HPP__INCLUDED

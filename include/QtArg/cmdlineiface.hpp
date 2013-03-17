
/*!
	\file

	\brief Command Line Parser.

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

#ifndef QTARG__CMDLINEIFACE_HPP__INCLUDED
#define QTARG__CMDLINEIFACE_HPP__INCLUDED


// Qt include.
#include <QtCore/QChar>
#include <QtCore/QList>


class QtArgIface;


//
// QtArgCmdLineIface
//

/*!
	\class QtArgCmdLineIface
	\brief Command line arguments parser interface.

	This is an interface for command line parser. In usual cases
	you don't nedd to implement this interface, because there are
	always ready implementation of this interface that handles
	various situations.

	\sa QtArgCmdLine
*/
class QtArgCmdLineIface {
	protected:
		virtual ~QtArgCmdLineIface()
		{}

	public:
		//! Type of the list of the arguments in the command line.
		typedef QList< QtArgIface* > QtArgumentsList;

		/*!
			\return List of arguments handled by this command
			line interface.

			\sa QtArgCmdLine::arguments()
		*/
		virtual const QtArgumentsList & arguments() const = 0;

		/*!
			\return Delimiter char.

			By default used '-' as delimiter.

			\sa QtArgCmdLine::delimiter()
		*/
		virtual const QChar & delimiter() const = 0;

		/*!
			Set delimiter char.

			\sa QtArgCmdLine::setDelimiter()
		*/
		virtual void setDelimiter( const QChar & delim ) = 0;

		/*!
			Add new argument for handling by this command line
			interface.

			\sa QtArgCmdLine::addArg()
		*/
		virtual void addArg( QtArgIface & arg ) = 0;

		/*!
			Add new argument for handling by this command line
			interface.

			\sa QtArgCmdLine::addArg()
		*/
		virtual void addArg( QtArgIface * arg ) = 0;

		/*!
			Parse arguments.

			Usually this is very difficult process.

			\sa QtArgCmdLine::parse()
		*/
		virtual void parse( bool parseAfterIgnoreRest = false ) = 0;

		/*!
			\return Is \par str an flag or string of the flags.
			\retval true if \par str is an flag or string of the flags.
			\retval false if \par str is not a flag or string of the flags.
		*/
		virtual bool isFlag( const QString & str ) const = 0;

		/*!
			\return Is \par str an argument.
			\retval true if \par str is an argument.
			\retval false if \par str is not an argument.
		*/
		virtual bool isArgument( const QString & str ) const = 0;
}; // class QtArgCmdLineIface

#endif // QTARG__CMDLINEIFACE_HPP__INCLUDED

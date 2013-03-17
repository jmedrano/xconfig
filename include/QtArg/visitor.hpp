
/*!
	\file

	\brief Interface for the visitors of the QtArg.

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

#ifndef QTARG__VISITOR_HPP__INCLUDED
#define QTARG__VISITOR_HPP__INCLUDED


// QtArg include.
#include "cmdlinecontext.hpp"


//
// QtArgVisitorIface
//

/*!
	\class QtArgVisitorIface
	\brief Interface for the visitors of the QtArg.

	Visitors are usefull for handling "non-standard"
	situations with argument's values.

	For example if you want to have argument with two
	ore more (list)	of values. Here visitors are very
	suitable. But for this task you have to derive
	subclass from QtArg or whatever derived from this one.

	Look at the example:

\code
class MyArg;

// Exception while processing argument with two values.
class MyArgException
  :  public QtArgBaseException
{
public:
  explicit MyArgException( const QString & desc )
	:  QtArgBaseException( desc )
  {
  }
}; // class MyArgException

// Visitor for argument with two values.
class MyVisitor
  :  public QtArgVisitorIface
{
public:
  MyVisitor()
	:  m_arg( 0 )
  {
  }

  // Set parent argument.
  void setArg( MyArg * arg )
  {
	m_arg = arg;
  }

  // Process with visitor.
  void visit( QtArgCmdLineContext & context )
  {
	if( !m_arg )
	  throw MyArgException( QLatin1String( "Argument wasn't defined." ) );

	if( !context.atEnd() )
	  m_arg->setSecondValue( context.next() );
	else
	  throw MyArgException( QLatin1String( "There is no second value "
		"for the MyArg argument." ) );
  }

private:
  // Parent argument.
  MyArg * m_arg;
}; // class MyVisitor

// Argument with two values.
class MyArg
  :  public QtArg
{
public:
  MyArg()
	:  QtArg( 'a', QLatin1String( "arg" ),
		  QLatin1String( "This is our argument with two required values" ),
		  true, true )
  {
	m_visitor.setArg( this );
	setVisitor( &m_visitor );
  }

  // Set second value.
  void setSecondValue( const QVariant & val )
  {
	m_secondValue = val;
  }

  // Second value.
  const QVariant & secondValue() const
  {
	return m_secondValue;
  }

private:
  // Visitor.
  MyVisitor m_visitor;
  // Second value.
  QVariant m_secondValue;
}; // class MyArg
\endcode

	In this example argument [-a, --arg] require two values.
	It's mean if after [--agr] argument will only one value
	than exception MyArgException will thrown.

	For example: [-a value1 value2] is correct, when
	[-a value1] is incorrect.
*/
class QtArgVisitorIface {
	protected:
		virtual ~QtArgVisitorIface() {}

	public:
		//! Process with visitor.
		/*!
			In this method visitor should process with command line
			context in that way that developer need.

			Use QtArgCmdLineContext::next() method to get next string in
			the command line. But first of all you need to check if
			there is strings available in the command line with
			QtArgCmdLineContext::atEnd() method.

			Command line context passed as argument to this method.
		*/
		virtual void visit( QtArgCmdLineContext & ) {}
}; // class QtArgVisitorIface

#endif // QTARG__VISITOR_HPP__INCLUDED

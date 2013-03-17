
/*!
	\file

	\brief Command Line Argument.

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

#ifndef QTARG__ARG_HPP__INCLUDED
#define QTARG__ARG_HPP__INCLUDED

// Qt include.
#include <QtCore/QChar>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QVariant>
#include <QtCore/QRegExp>
#include <QtCore/QList>

// QtArg include.
#include "exceptions.hpp"
#include "cmdlinecontext.hpp"
#include "visitor.hpp"
#include "argconstraint.hpp"
#include "cmdlineiface.hpp"
#include "helpiface.hpp"


class QtArgCmdLine;

//
// class QtArgIface
//

/*!
	\class QtArgIface
	\brief Interface of the argument.

	Should be implemented by every class wanted
	to be an argument.
*/
class QtArgIface {

	friend class QtArgCmdLine;

	public:
		//! Type of the list of argument's names.
		typedef QStringList NamesList;
		//! Type of the list of argument's flags.
		typedef QList< QChar > FlagsList;

	protected:
		QtArgIface()
		{}

		QtArgIface(
			//! Names of the argument.
			const NamesList & names,
			//! Flags of the argument.
			const FlagsList & flags )
			:	m_flags( flags )
			,	m_names( names )
		{}

		virtual ~QtArgIface()
		{}

	public:
		/*!
			\return Is this argument required?
			\retval true if argument is required.
			\retval false if argument isn't required.
		*/
		virtual bool isRequired() const = 0;

		/*!
			\return Is this argument defined in command line?
			\retval true if argument has decided he has been defined.
			\retval false if argument hasn't decided he has been defined..
		*/
		virtual bool isDefined() const = 0;

		/*!
			\return Whether this argument should have a value or not.
			\retval true if this argument should have a value.
			\retval false if this argument shouldn't have a value.
		*/
		virtual bool isWithValue() const = 0;

		/*!
			Check correctness of the names, flags and so on before parsing.

			\throw QtArgDissallowedFlagOrNameEx
			\throw QtArgNameOrFlagAlreadyKnownEx
		*/
		virtual void check(
			//! Delimiter character.
			const QChar & delimiter,
			//! All known flags in every argument.
			FlagsList & alreadyKnownFlags,
			//! All known names in every argument.
			NamesList & alreadyKnownNames ) const = 0;

		//! Check correctness of the state of the argument after parsing.
		virtual void check() const = 0;

		//! \return All flags of the argument.
		virtual const FlagsList & flags() const;

		//! \return All names of the argument.
		virtual const NamesList & names() const;

		//! \return Description of the argument.
		virtual QString description() const = 0;

		/*!
			If long description is empty it's better to return
			description().

			\return Long descrition for this argument.
		*/
		virtual QString longDescription() const = 0;

		/*!
			\brief Should return pointer to the QtArgIface if this
			argument know how to do it, usually returns "this" if he does.
			If this argument handles another arguments with flag \a flag
			than he should return pointer to this argument.

			\retval Pointer to the QtArgIface if flag \a flag determines
			this argument.
			\retval NULL if flag \a flag doesn't determine this argument.
		*/
		virtual QtArgIface * giveArgument(
			/*!
				Is a flag that returned argument should own. If
				this argument doesn't own this flag or sub-arguments don't
				own this flag then returned value should be null-pointer.
			*/
			const QChar & flag ) = 0;

		/*!
			\brief Should return pointer to the QtArgIface if this
			argument know how to do it, usually returns "this" if he does.
			If this argument handles another arguments with name \a name
			than he should return pointer to this argument.

			\retval Pointer to the QtArgIface if name \a name determines
			this argument.
			\retval NULL if name \a name doesn't determine this argument.
		*/
		virtual QtArgIface * giveArgument(
			/*!
				Is a name that returned argument should own. If
				this argument doesn't own this name or sub-arguments don't
				own this name then returned value should be null-pointer.
			*/
			const QString & name ) = 0;

		/*!
			\brief Usually returns list with "this" pointer or list
			with all handled arguments.

			\return List of the arguments handled by this argument.
		*/
		virtual const QtArgCmdLineIface::QtArgumentsList &
		arguments() const = 0;

		/*!
			\brief Check correctness of the argument constraint.

			\throw QtArgContraintNotObservedEx if constraint
			didn't observed.
		*/
		virtual void checkConstraint() const = 0;

		/*!
			Should return "USAGE" string for this argument.
			For example: "-a%afterFlags%--arg %value%", where
			"%value%" is QtArgHelpPrinterIface::argValue and
			"%afterFlags%" is QtArgHelpPrinterIface::afterFlags.

			\return "USAGE" string for this argument.
		*/
		virtual QString getUsageString(
			//! Names of all available arguments.
			const NamesList & namesList ) const;

		/*!
			Should return "HELP" string for this argument.

			For spaces you have to use string constants from
			QtArgHelpPrinterIface interface.

			\return "HELP" string for this argument.
		*/
		virtual QString getHelpString(
			//! Names of all available arguments.
			const NamesList & namesList ) const;

	protected:
		/*!
			\brief Process argument.

			Can throw exceptions.

			\return The number of processed arguments.
		*/
		virtual int process(
			//! Command line context.
			QtArgCmdLineContext & context ) = 0;

		//! Process with visitor.
		virtual void visit(
			//! Command line context.
			QtArgCmdLineContext & context ) = 0;

	protected:
		//! Flag of the argument. If defined.
		FlagsList m_flags;
		//! List of the names of the argument.
		NamesList m_names;
}; // class QtArgIface


//
// QtArgIface implementation.
//

inline const QtArgIface::FlagsList &
QtArgIface::flags() const
{
	return m_flags;
}

inline const QtArgIface::NamesList &
QtArgIface::names() const
{
	return m_names;
}

inline QString
QtArgIface::getUsageString( const NamesList & namesList ) const
{
	QString usage;

	if( flags().size() )
	{
		usage.append( QtArgHelpPrinterIface::beforeFlags );
		usage.append( QtArgHelpPrinterIface::flagMarker );

		foreach( QChar f, flags() )
			usage.append( f );
	}

	if( names().size() )
	{
		if( flags().size() )
			usage.append( QtArgHelpPrinterIface::afterFlags );
		else
			usage.append( QtArgHelpPrinterIface::beforeNames );

		bool first = true;

		foreach( QString name, names() )
		{
			if( !first )
				usage.append( QtArgHelpPrinterIface::namesSeparator );
			else
				first = false;

			usage.append( QtArgHelpPrinterIface::nameMarker );
			markUnrequiredPartOfTheName( name, namesList );
			usage.append( name );
		}

		if( isWithValue() )
			usage.append( QtArgHelpPrinterIface::argValue );
	}
	else
		if( isWithValue() )
			usage.append( QtArgHelpPrinterIface::argValue );

	return usage;
}

inline QString
QtArgIface::getHelpString( const NamesList & namesList ) const
{
	QString help = getUsageString( namesList );
	help.append( QtArgHelpPrinterIface::beforeDescription );
	help.append( description() );
	help.append( QtArgHelpPrinterIface::newLine );

	return help;
}


//
// class QtArg
//

/*!
	\class QtArg
	\brief Base class of the command line argument.

	This is an argument with one parameter or without
	any parameters.
*/
class QtArg
	:	public QtArgIface
{
	public:
		explicit QtArg(
			//! Flag of the argument.
			const QChar & flag,
			//! Name of the argument.
			const QString & name = QString(),
			//! Description of the argument.
			const QString & description = QString(),
			//! Is argument required?
			bool required = false,
			//! Should argument has a value?
			bool withValue = false,
			//! Visitor of the argument.
			QtArgVisitorIface * visitor = NULL,
			//! Constraint for the argument's values.
			QtArgConstraintIface * constraint = NULL );

		QtArg(
			//! Flag of the argument.
			const QChar & flag,
			//! Various names of the argument.
			const QtArgIface::NamesList & names,
			//! Description of the argument.
			const QString & description = QString(),
			//! Is argument required?
			bool required = false,
			//! Should argument has a value?
			bool withValue = false,
			//! Visitor of the argument.
			QtArgVisitorIface * visitor = NULL,
			//! Constraint for the argument's values.
			QtArgConstraintIface * constraint = NULL );

		explicit QtArg(
			//! Name of the argument.
			const QString & name,
			//! Description of the argument.
			const QString & description = QString(),
			//! Is argument required?
			bool required = false,
			//! Should argument has a value?
			bool withValue = false,
			//! Visitor of the argument.
			QtArgVisitorIface * visitor = NULL,
			//! Constraint for the argument's values.
			QtArgConstraintIface * constraint = NULL );

		explicit QtArg(
			//! Various names of the argument.
			const QtArgIface::NamesList & names,
			//! Description of the argument.
			const QString & description = QString(),
			//! Is argument required?
			bool required = false,
			//! Should argument has a value?
			bool withValue = false,
			//! Visitor of the argument.
			QtArgVisitorIface * visitor = NULL,
			//! Constraint for the argument's values.
			QtArgConstraintIface * constraint = NULL );

		explicit QtArg(
			//! Various flags of the argument.
			const QtArgIface::FlagsList & flags,
			//! Description of the argument.
			const QString & description = QString(),
			//! Is argument required?
			bool required = false,
			//! Should argument has a value?
			bool withValue = false,
			//! Visitor of the argument.
			QtArgVisitorIface * visitor = NULL,
			//! Constraint for the argument's values.
			QtArgConstraintIface * constraint = NULL );

		QtArg(
			//! Various flags of the argument.
			const QtArgIface::FlagsList & flags,
			//! Various names of the argument.
			const QtArgIface::NamesList & names,
			//! Description of the argument.
			const QString & description = QString(),
			//! Is argument required?
			bool required = false,
			//! Should argument has a value?
			bool withValue = false,
			//! Visitor of the argument.
			QtArgVisitorIface * visitor = NULL,
			//! Constraint for the argument's values.
			QtArgConstraintIface * constraint = NULL );

		QtArg( const QtArg & arg );
		QtArg & operator = ( const QtArg & arg );

		virtual ~QtArg();

		/*!
			\return Flag of the argument. Returned value is a first
			flag from the list of flags if defined more than one.
		*/
		virtual QChar flag() const;

		/*!
			\brief Set new value of the flag of the argument.

			Clears all previously defined flags from the list
			and set this new value.
		*/
		virtual void setFlag(
			//! New value of the flag.
			const QChar & fl );

		/*!
			\brief Set new value of the flag's list of the argument.

			Clears all previously defined flags from the list
			and set this new values.
		*/
		virtual void setFlags(
			//! New value of the flag's list.
			const QtArgIface::FlagsList & f );

		/*!
			\return Name of the argument. Returned value is a first
			name from the list of names if defined more than one.
		*/
		QString name() const;

		/*!
			\brief Set new value of the name of the argument.

			Clears all previously defined names from the list
			and set this new value.
		*/
		void setName(
			//! New value of the name.
			const QString & n );

		/*!
			\brief Set new value of the name of the argument.

			Clears all previously defined names from the list
			and set this new values.
		*/
		void setNames(
			//! List of names.
			const QtArgIface::NamesList & n );

		/*!
			\brief Set value of the description of the argument.

			Rewrites previously defined description.
		*/
		void setDescription(
			//! Description of the argument.
			const QString & ds );

		/*!
			\brief Set long description for this argument.

			Rewrites previously defined long description.
		*/
		void setLongDescription(
			//! Long description of the argument.
			const QString & desc );

		//! \brief Set value of the required property.
		void setRequired(
			//! Value of the "required" property.
			bool on = true );

		/*!
			\brief Set value of the property that hold whether
			this argument should have a value or not.
		*/
		void setWithValue(
			//! Value of the "withValue" property.
			bool on = true );

		/*!
			\return Visitor of this argument,
			\retval NULL if visitor wasn't defined.
		*/
		QtArgVisitorIface * visitor() const;

		/*!
			\brief Set visitor to this argument.

			Replace previously defined visitor if defined.
		*/
		void setVisitor(
			//! Pointer to the visitor.
			QtArgVisitorIface * v );

		/*!
			\return Constraint for the values,
			\retval NULL if constraint wasn't defined.
		*/
		QtArgConstraintIface * constraint() const;

		/*!
			\brief Set constraint to this argument.

			Replace previously defined constraint if defined.
		*/
		void setConstraint(
			//! Pointer to the constraint.
			QtArgConstraintIface * c );

		//! \return Value of the argument.
		const QVariant & value() const;

		//! \return Value of the argument.
		QVariant & value();

		//! \brief Set value of the argument.
		virtual void setValue(
			//! Value of the argument.
			const QVariant & v );

		//! \brief Set "defined" property.
		void setDefined(
			//! Value of the "defined" property.
			bool on = true );

		/*!
			\retval true if argument was present in the command line.
			\retval false if argument wasn't present in the command line.
		*/
		bool isPresent() const;

		//! Set "present" property.
		void setPresent(
			//! Value of the "present" property.
			bool on = true );

		//! \name QtArgIface implementation.
		//! \{

			/*!
				\return Is this argument required?
				\retval true if argument is required.
				\retval false if argument isn't required.
			*/
			virtual bool isRequired() const;

			/*!
				\return Is this argument defined in command line?
				\retval true if argument has decided he has been defined.
				\retval false if argument hasn't decided he has been defined..
			*/
			virtual bool isDefined() const;

			/*!
				\return Whether this argument should have a value or not.
				\retval true if this argument should have a value.
				\retval false if this argument shouldn't have a value.
			*/
			virtual bool isWithValue() const;

			/*!
				Check correctness of the names, flags and so on before parsing.

				\throw QtArgDissallowedFlagOrNameEx
				\throw QtArgNameOrFlagAlreadyKnownEx
			*/
			virtual void check(
				//! Delimiter character.
				const QChar & delimiter,
				//! All known flags in every argument.
				FlagsList & alreadyKnownFlags,
				//! All known names in every argument.
				NamesList & alreadyKnownNames ) const;

			//! Check correctness of the state of the argument after parsing.
			virtual void check() const;

			//! \return Description of the argument.
			virtual QString description() const;

			/*!
				If long description is empty it's better to return
				description().

				\return Long descrition for this argument.
			*/
			virtual QString longDescription() const;

			/*!
				\brief Should return pointer to the QtArgIface if this
				argument know how to do it, usually returns "this" if he does.
				If this argument handles another arguments with flag \a flag
				than he should return pointer to this argument.

				\retval Pointer to the QtArgIface if flag \a flag determines
				this argument.
				\retval NULL if flag \a flag doesn't determine this argument.
			*/
			virtual QtArgIface * giveArgument(
				/*!
					Is a flag that returned argument should own. If
					this argument doesn't own this flag or sub-arguments don't
					own this flag then returned value should be null-pointer.
				*/
				const QChar & flag );

			/*!
				\brief Should return pointer to the QtArgIface if this
				argument know how to do it, usually returns "this" if he does.
				If this argument handles another arguments with name \a name
				than he should return pointer to this argument.

				\retval Pointer to the QtArgIface if name \a name determines
				this argument.
				\retval NULL if name \a name doesn't determine this argument.
			*/
			virtual QtArgIface * giveArgument(
				/*!
					Is a name that returned argument should own. If
					this argument doesn't own this name or sub-arguments don't
					own this name then returned value should be null-pointer.
				*/
				const QString & name );

			/*!
				\brief Usually returns list with "this" pointer or list
				with all handled arguments.

				\return List of the arguments handled by this argument.
			*/
			virtual const QtArgCmdLineIface::QtArgumentsList &
			arguments() const;

			/*!
				\brief Check correctness of the argument constraint.

				\throw QtArgContraintNotObservedEx if constraint
				didn't observed.
			*/
			virtual void checkConstraint() const;

	protected:
			/*!
				\brief Process argument.

				Can throw exceptions.

				\return The number of processed arguments.
			*/
			virtual int process(
				//! Command line context.
				QtArgCmdLineContext & context );

			//! Process with visitor.
			virtual void visit(
				//! Command line context.
				QtArgCmdLineContext & context );

		//! \}

		//! Is equal with another argument?
		friend bool operator == ( const QtArg & a1, const QtArg & a2 );
		//! Isn't equal with another argument?
		friend bool operator != ( const QtArg & a1, const QtArg & a2 );
		//! Is equal with name \a a2?
		friend bool operator == ( const QtArg & a1, const QString & a2 );
		//! Isn't equal with name \a a2?
		friend bool operator != ( const QtArg & a1, const QString & a2 );
		//! Is equal with flag \a a2?
		friend bool operator == ( const QtArg & a1, const QChar & a2 );
		//! Isn't equal with flag \a a2?
		friend bool operator != ( const QtArg & a1, const QChar & a2 );
		//! Is equal with flag's list \a a2?
		friend bool operator == ( const QtArg & a1,
			const QtArgIface::FlagsList & a2 );
		//! Isn't equal with flag's list \a a2?
		friend bool operator != ( const QtArg & a1,
			const QtArgIface::FlagsList & a2 );
		//! Is equal with name's list \a a2?
		friend bool operator == ( const QtArg & a1,
			const QtArgIface::NamesList & a2 );
		//! Isn't equal with name's list \a a2?
		friend bool operator != ( const QtArg & a1,
			const QtArgIface::NamesList & a2 );

	private:
		//! Description of the argument.
		QString m_description;
		//! Long description for this argument.
		QString m_longDescription;

		//! Is argument required? "required" property.
		bool m_required;
		//! Should argument has a value? "withValue" property.
		bool m_withValue;
		//! Is this argument and probaly his value defined. "defined" property.
		bool m_defined;
		//! Was this argument present in the command line. "present" property.
		bool m_present;

		//! Value of the argument.
		QVariant m_value;

		/*!
			\brief Visitor of the argument.

			QtArg doesn't handle life cycle of the
			QtArgVisitorIface. So you must care about it.
		*/
		QtArgVisitorIface * m_visitor;

		/*!
			\brief Constraint foor the argument's values.

			QtArg doesn't handle life cycle of the
			QtArgConstraintIface. So you must care about it.
		*/
		QtArgConstraintIface * m_constraint;

		//! Arguments handled by this argument.
		QtArgCmdLineIface::QtArgumentsList m_args;
}; // class QtArg


//
// QtArg implementation.
//

inline
QtArg::QtArg(
	const QChar & flag,
	const QString & name,
	const QString & description,
	bool required,
	bool withValue,
	QtArgVisitorIface * visitor,
	QtArgConstraintIface * constraint )
		:	QtArgIface( name.length() ?
				QtArgIface::NamesList() << name : QtArgIface::NamesList(),
				QtArgIface::FlagsList() << flag )
		,	m_description( description )
		,	m_required( required )
		,	m_withValue( withValue )
		,	m_defined( false )
		,	m_present( false )
		,	m_visitor( visitor )
		,	m_constraint( constraint )
{
	m_args << this;
}

inline
QtArg::QtArg(
	const QChar & flag,
	const QtArgIface::NamesList & names,
	const QString & description,
	bool required,
	bool withValue,
	QtArgVisitorIface * visitor,
	QtArgConstraintIface * constraint )
		:	QtArgIface( names, QtArgIface::FlagsList() << flag )
		,	m_description( description )
		,	m_required( required )
		,	m_withValue( withValue )
		,	m_defined( false )
		,	m_present( false )
		,	m_visitor( visitor )
		,	m_constraint( constraint )
{
	m_args << this;
}

inline
QtArg::QtArg(
	const QString & name,
	const QString & description,
	bool required,
	bool withValue,
	QtArgVisitorIface * visitor,
	QtArgConstraintIface * constraint )
		:	QtArgIface( QtArgIface::NamesList() << name,
				QtArgIface::FlagsList() )
		,	m_description( description )
		,	m_required( required )
		,	m_withValue( withValue )
		,	m_defined( false )
		,	m_present( false )
		,	m_visitor( visitor )
		,	m_constraint( constraint )
{
	m_args << this;
}

inline
QtArg::QtArg(
	const QtArgIface::NamesList & names,
	const QString & description,
	bool required,
	bool withValue,
	QtArgVisitorIface * visitor,
	QtArgConstraintIface * constraint )
		:	QtArgIface( names, QtArgIface::FlagsList() )
		,	m_description( description )
		,	m_required( required )
		,	m_withValue( withValue )
		,	m_defined( false )
		,	m_present( false )
		,	m_visitor( visitor )
		,	m_constraint( constraint )
{
	m_args << this;
}

inline
QtArg::QtArg(
	const QtArgIface::FlagsList & flags,
	const QString & description,
	bool required,
	bool withValue,
	QtArgVisitorIface * visitor,
	QtArgConstraintIface * constraint )
		:	QtArgIface( QtArgIface::NamesList(), flags )
		,	m_description( description )
		,	m_required( required )
		,	m_withValue( withValue )
		,	m_defined( false )
		,	m_present( false )
		,	m_visitor( visitor )
		,	m_constraint( constraint )
{
	m_args << this;
}

inline
QtArg::QtArg(
	const QtArgIface::FlagsList & flags,
	const QtArgIface::NamesList & names,
	const QString & description,
	bool required,
	bool withValue,
	QtArgVisitorIface * visitor,
	QtArgConstraintIface * constraint )
		:	QtArgIface( names, flags )
		,	m_description( description )
		,	m_required( required )
		,	m_withValue( withValue )
		,	m_defined( false )
		,	m_present( false )
		,	m_visitor( visitor )
		,	m_constraint( constraint )
{
	m_args << this;
}

inline
QtArg::QtArg( const QtArg & arg )
	:	QtArgIface( arg.m_names, arg.m_flags )
	,	m_description( arg.m_description )
	,	m_required( arg.m_required )
	,	m_withValue( arg.m_withValue )
	,	m_defined( arg.m_defined )
	,	m_present( arg.m_present )
	,	m_value( arg.m_value )
	,	m_visitor( arg.m_visitor )
	,	m_constraint( arg.m_constraint )
{
	m_args << this;
}

inline QtArg &
QtArg::operator = ( const QtArg & arg )
{
	m_flags = arg.m_flags;
	m_names = arg.m_names;
	m_description = arg.m_description;
	m_required = arg.m_required;
	m_withValue = arg.m_withValue;
	m_visitor = arg.m_visitor;
	m_defined = arg.m_defined;
	m_present = arg.m_present;
	m_value = arg.m_value;
	m_constraint = arg.m_constraint;

	return *this;
}

inline
QtArg::~QtArg()
{
}

inline QChar
QtArg::flag() const
{
	if( !m_flags.isEmpty() )
		return m_flags.front();
	else
		return QChar();
}

inline void
QtArg::setFlag( const QChar & fl )
{
	m_flags.clear();
	m_flags << fl;
}

inline void
QtArg::setFlags( const QtArgIface::FlagsList & f )
{
	m_flags = f;
}

inline QString
QtArg::name() const
{
	if( !m_names.isEmpty() )
		return m_names.front();
	else
		return QString();
}

inline void
QtArg::setName( const QString & n )
{
	m_names.clear();
	m_names << n;
}

inline void
QtArg::setNames( const QtArgIface::NamesList & n )
{
	m_names = n;
}


inline QString
QtArg::description() const
{
	return m_description;
}

inline void
QtArg::setDescription( const QString & ds )
{
	m_description = ds;
}

inline QString
QtArg::longDescription() const
{
	return m_longDescription;
}

inline void
QtArg::setLongDescription( const QString & desc )
{
	m_longDescription = desc;
}

inline bool
QtArg::isRequired() const
{
	return m_required;
}

inline void
QtArg::setRequired( bool on )
{
	m_required = on;
}

inline bool
QtArg::isWithValue() const
{
	return m_withValue;
}

inline void
QtArg::setWithValue( bool on )
{
	m_withValue = on;
}


inline QtArgVisitorIface *
QtArg::visitor() const
{
	return m_visitor;
}

inline void
QtArg::setVisitor( QtArgVisitorIface * v )
{
	m_visitor = v;
}

inline QtArgConstraintIface *
QtArg::constraint() const
{
	return m_constraint;
}

inline void
QtArg::setConstraint( QtArgConstraintIface * c )
{
	m_constraint = c;
}

inline const QVariant &
QtArg::value() const
{
	return m_value;
}

inline QVariant &
QtArg::value()
{
	return m_value;
}

inline void
QtArg::setValue( const QVariant & v )
{
	m_value = v;
	setDefined();
}

inline bool
QtArg::isDefined() const
{
	return m_defined;
}

inline void
QtArg::setDefined( bool on )
{
	m_defined = on;

	setPresent();
}

inline bool
QtArg::isPresent() const
{
	return m_present;
}

inline void
QtArg::setPresent( bool on )
{
	m_present = on;
}

inline QtArgIface *
QtArg::giveArgument( const QChar & f )
{
	if( !m_flags.isEmpty() )
		foreach( QChar flag, m_flags )
			if( flag == f )
				return this;

	return NULL;
}

inline QtArgIface *
QtArg::giveArgument( const QString & n )
{
	if( m_names.size() )
		foreach( QString name, m_names )
			if( name.startsWith( n ) )
				return this;

	return NULL;
}

inline const QtArgCmdLineIface::QtArgumentsList &
QtArg::arguments() const
{
	return m_args;
}

inline void
QtArg::check(
	const QChar & delimiter,
	QtArgIface::FlagsList & alreadyKnownFlags,
	QtArgIface::NamesList & alreadyKnownNames ) const
{
	if( m_flags.isEmpty() && m_names.isEmpty() )
		throw QtArgNotDefinedNorNameNorFlagEx();

	if( m_flags.size() )
	{
		foreach( QChar flag, m_flags )
		{
			if( delimiter == flag || !flag.isLetterOrNumber() )
				throw QtArgDissallowedFlagOrNameEx(
					QString::fromLatin1( "Dissallowed flag: \"%1\"." )
						.arg( flag ) );

			if( alreadyKnownFlags.contains( flag ) )
				throw QtArgNameOrFlagAlreadyKnownEx(
					QString::fromLatin1( "Flag already in use: %1." )
						.arg( flag ) );

			alreadyKnownFlags.append( flag );
		}
	}

	if( m_names.size() )
	{
		foreach( QString name, m_names )
		{
			if( name.isEmpty() ||
				name.contains( QRegExp( QLatin1String( "\\s" ) ) ) )
					throw QtArgDissallowedFlagOrNameEx(
						QString::fromLatin1( "Dissallowed name: \"%1\"." )
							.arg( name ) );

			if( alreadyKnownNames.contains( name ) )
				throw QtArgNameOrFlagAlreadyKnownEx(
					QString::fromLatin1( "Name already in use: %1." )
						.arg( name ) );

			alreadyKnownNames.append( name );
		}
	}
}

inline void
QtArg::check() const
{
	if( !isDefined() )
	{
		if( isRequired() && !isPresent() )
			throw QtArgNotDefinedMandatoryArgumentEx(
				QString::fromLatin1( "Not defined mandatory argument: %1" )
					.arg( names().size() ? names().front() : flags().front() ) );

		if( isPresent() && isWithValue() )
			throw QtArgNotDefinedOptionForTheArgumentEx(
				QString::fromLatin1( "Not defined value of the argument: %1" )
					.arg( names().size() ? names().front() : flags().front() ) );
	}
}

inline int
QtArg::process( QtArgCmdLineContext & context )
{
	if( isWithValue() )
	{
		if( !context.atEnd() )
		{
			setValue( context.next() );

			return 1;
		}
		else
			setPresent();
	}
	else
		setValue( QVariant( true ) );

	return 0;
}

inline void
QtArg::visit( QtArgCmdLineContext & context )
{
	if( visitor() ) visitor()->visit( context );
}

inline void
QtArg::checkConstraint() const
{
	if( m_constraint && isDefined() )
		if( !m_constraint->check( m_value ) )
			throw QtArgContraintNotObservedEx(
				QString::fromLatin1( "Constraint for the argument: %1"
				" hasn't observed. Wrong value is: %2" )
					.arg( names().size() ? names().front() : flags().front() )
					.arg( m_value.toString() ) );
}


namespace /* anonymous */ {

	inline bool
	isNamesEqual( const QtArgIface::NamesList & l1,
		const QtArgIface::NamesList & l2 )
	{
		if( l1.size() == l2.size() )
		{
			foreach( QString name, l1 )
				if( !l2.contains( name ) )
					return false;
		}
		else
			return false;

		return true;
	}

	inline bool
	isFlagsEqual( const QtArgIface::FlagsList & f1,
		const QtArgIface::FlagsList & f2 )
	{
		if( f1.size() == f2.size() )
		{
			foreach( QChar flag, f1 )
				if( !f2.contains( flag ) )
					return false;
		}
		else
			return false;

		return true;
	}

} /* namespace anonymous */

inline bool operator == ( const QtArg & a1, const QtArg & a2 )
{
	if( a1.m_flags.isEmpty() && a2.m_flags.isEmpty() )
		return isNamesEqual( a1.m_names, a2.m_names );
	else if( !a1.m_flags.isEmpty() && !a2.m_flags.isEmpty() )
	{
		if( isFlagsEqual( a1.m_flags, a2.m_flags ) )
			return isNamesEqual( a1.m_names, a2.m_names );
		else
			return false;
	}
	else
		return false;
}

inline bool operator != ( const QtArg & a1, const QtArg & a2 )
{
	return !( a1 == a2 );
}

inline bool operator == ( const QtArg & a1, const QString & a2 )
{
	QtArg tmp = a1;
	tmp.setName( a2 );

	return ( a1 == tmp );
}

inline bool operator != ( const QtArg & a1, const QString & a2 )
{
	QtArg tmp = a1;
	tmp.setName( a2 );

	return ( a1 != tmp );
}

inline bool operator == ( const QtArg & a1, const QChar & a2 )
{
	QtArg tmp = a1;
	tmp.setFlag( a2 );

	return ( a1 == tmp );
}

inline bool operator != ( const QtArg & a1, const QChar & a2 )
{
	QtArg tmp = a1;
	tmp.setFlag( a2 );

	return ( a1 != tmp );
}

inline bool operator == ( const QtArg & a1, const QtArgIface::FlagsList & a2 )
{
	QtArg tmp = a1;
	tmp.setFlags( a2 );

	return ( a1 == tmp );
}

inline bool operator != ( const QtArg & a1, const QtArgIface::FlagsList & a2 )
{
	QtArg tmp = a1;
	tmp.setFlags( a2 );

	return ( a1 != tmp );
}

inline bool operator == ( const QtArg & a1, const QtArgIface::NamesList & a2 )
{
	QtArg tmp = a1;
	tmp.setNames( a2 );

	return ( a1 == tmp );
}

inline bool operator != ( const QtArg & a1, const QtArgIface::NamesList & a2 )
{
	QtArg tmp = a1;
	tmp.setNames( a2 );

	return ( a1 != tmp );
}

#endif // QTARG__ARG_HPP__INCLUDED

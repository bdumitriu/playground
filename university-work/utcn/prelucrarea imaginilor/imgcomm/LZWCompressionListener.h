
#ifndef _LZWCOMPRESSIONLISTENER_H_
#define _LZWCOMPRESSIONLISTENER_H_

#define PERIODIC_COMPRESSION 0
#define CLEARING_TABLE 1
#define INCREMENTING_CODE_SIZE 2
#define OPERATION_TERMINATED 3
#define OPERATION_STARTED 4


 /**
 * The following is a class containing the event that has occured.
 *
 * Author Tudor Marian<br />
 *
 * Technical University Of Cluj-Napoca<br />
 * Computer Science Departament<br />
 * gr. 3241/1<br />
 *
 * created: april, 13, 2003<br />
 * last modified: april 19, 2003<br />
 *
 * @author	Tudor Marian
 */
class LZWCompressionEvent
{
public:
	/** The event type.
	*/
	int eventType;
	/**
	* Creates an event object.
	*
	* @param	eventType	the type of the event produced.
	*/
	LZWCompressionEvent(int eventType) {this->eventType = eventType;};

	/**
	* Destroys the event object.
	*/
	~LZWCompressionEvent(){};
};

/**
 * The following is an abstract class used by certain objects to register
 * for events poccured during the lzw compression-expansion procedure. <br /><br />
 *
 * Author Tudor Marian<br />
 *
 * Technical University Of Cluj-Napoca<br />
 * Computer Science Departament<br />
 * gr. 3241/1<br />
 *
 * created: april, 13, 2003<br />
 * last modified: april 19, 2003<br />
 *
 * @author	Tudor Marian
 */
class LZWCompressionListener
{
protected:	
	/**
	* Creates an object. The constructor is protected not to allow objects
	* of this direct class to be actually created.
	*/
	LZWCompressionListener() {};

	/**
	* This abstract destructor is supposed to destroy the object.
	*/
	virtual ~LZWCompressionListener() {};

public:

	/**
	* The method is used to be implemented by the classes that implement
	* this interface thus providing the callback routine.
	*/
	virtual void actionPerformed(LZWCompressionEvent* event) = 0;

};

#endif //_LZWCOMPRESSIONLISTENER_H_


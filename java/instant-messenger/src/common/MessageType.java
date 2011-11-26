package common;

/**
 * @author Bogdan Dumitriu
 * @author bdumitriu@bdumitiru.ro
 * @version 0.1
 * @date Sep 19, 2005
 */
public enum MessageType
{
	LOG_IN, LOG_OUT, GET_FRIENDS,
	MESSAGE,
	STATUS_UPDATE, FRIEND_LOGIN, FRIEND_LOGOUT,
	ADD_FRIEND, REMOVE_FRIEND,
	NOT_ONLINE, NOT_LOGGED_IN, NOT_A_USER, NOT_A_FRIEND, WRONG_USER_PASS, NOT_UNDERSTOOD,
	OK,
	PORT_INFO, SAVE_USER_LIST, FINISH_THREAD
}

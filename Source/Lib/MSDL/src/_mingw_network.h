#ifndef _x_mingw_network_h
#define _x_mingw_network_h

inline const char *inet_ntop(int af, const void *src, char *dst, socklen_t size)
{
	SOCKADDR address;
	DWORD address_length = 0;
	DWORD result_length = (DWORD) size;
	if ((af != AF_INET) && (af != AF_INET6))
		return 0;
	if (af == AF_INET)
	{
		address_length = sizeof(SOCKADDR_IN);
		SOCKADDR_IN *ip4 = (SOCKADDR_IN *) &address;
		ip4->sin_family = AF_INET;
		ip4->sin_port = 0;
		memcpy(&(ip4->sin_addr), src, sizeof(IN_ADDR));
	} 
	else // AF_INET6
	{
		address_length = sizeof(SOCKADDR_IN6);
		SOCKADDR_IN6 *ip6 = (SOCKADDR_IN6 *) &address;
		ip6->sin6_family = AF_INET6;
		ip6->sin6_port = 0;
		ip6->sin6_flowinfo = 0;
		memcpy(&(ip6->sin6_addr), src, sizeof(IN6_ADDR));
		ip6->sin6_scope_id = 0;
	}
	if (WSAAddressToString(&address, address_length, NULL, dst, &result_length))
		return 0;
	else
		return dst;
}

#endif

#include "Repeat.h"
// using namespace std;
namespace iret {

  /*
    Circular array iterator
    
    Index into a "circular array" that behaves somewhat like STL
    iterators. 
  
    circular array: advancing past the end of the array returns you back
    to the begining
    
    Only the needed portion of the iterator interface is implemented
  */
  
  template<typename T>
    class circular_array_iterator {
    public:
      circular_array_iterator( size_t size, T array[],
			       size_t index = 0 ) :
	my_size(size),
	my_array(array),
	my_index(index) {}
      void operator++(void) { my_index = (++my_index) % my_size; }
      T& operator*(void) { return *(my_array+my_index); }
      bool operator==( const circular_array_iterator<T> & other ) {
	return my_array == other.my_array && my_index == other.my_index;
      }
      
    private:
      T* my_array;		// the circular array
      size_t my_index;		// index into array
      size_t my_size;		// size of array
    };
  

  /** A string and a weight.

      Just keeps pointer to string, to be as light-weight as possible.
  */

  struct counted_string {
    counted_string() : strp(NULL), my_count(0) {}
    counted_string( Count* pct ) {
      strp = pct->show_str();
      my_count = pct->count();
    }
    counted_string operator = ( Count * pct ) {
      strp = pct->show_str();
      my_count = pct->count();
      return *this;
    }
    int count(void) { return my_count; }
    char* strp;			// string
    int my_count;			// count
  };


  /** Queue that also tracks the total of element counts.

      Elements must implement the count() method.
  */

  template<class T>
    class counted_queue {
    public:
      counted_queue( size_t size = 2 ) :
	my_count(0),
	my_max(size),
	my_size(0),
	my_array(new T[size]),
	my_front(size,my_array),
	my_back(my_front)
      { ++my_front;}

      void resize( size_t new_size ) {
	// also, reset the pointers and sizes
	my_count = 0;
	my_size = 0;

	if ( new_size != my_max ) {
	  // resize allocated space only if necessary

	  delete my_array;
	  my_array = new T[new_size];
	  my_max = new_size;
	  my_front = circular_array_iterator<T>(new_size,my_array);
	}

	my_back = my_front;
	++my_front;
      }

      /** Return true if the queue is full.
       */
      bool full(void) { return my_size >= my_max; }

      /** Remove the element at the front of the queue.  Return true
	  on success, false if queue was empty.
      */
      bool pop_front( void) {
	if ( empty() )
	  return false;
	
	my_count -= (*my_front).count();
	++my_front;
	--my_size;
	return true;
      }

      /** Add element to the back of the queue.  Return true on
	  success, false if queue was full.
      */
      bool push_back(T element) {
	if ( full() )
	  return false;
	
	++my_back;
	*my_back = element;
	my_count += (*my_back).count();
	++my_size;
	return true;
      }

      T& front( void ) { return *my_front; }
      T& back( void ) { return *my_back; }
      
      int count(void) { return my_count; }
      int size(void) { return my_size; }
      bool empty(void) { return my_size <= 0; }
      
    private:
      int my_count;
      int my_max;
      int my_size;
      
      T* my_array;
      circular_array_iterator<T> my_front;
      circular_array_iterator<T> my_back;
      
    };
  

  Repeat::Repeat() : Word() {
    init();
  }

  Repeat::Repeat(long wrd_space) : Word(wrd_space) {
    init();
  }

  Repeat::Repeat(long wrd_space, const char * lst_name) :
    Word(wrd_space,lst_name) {
    init();
  }

  Repeat::~Repeat(){
    delete pct;
    delete lst;
    delete my_queue;
  }

  void Repeat::init(void) {
    pct=new Count();
    lst=new List();
    
    byte_lim=0;
    back_lim=0;
    set_map(".,:;!?",'\024',LOWERCASE);
    pre_punct();
    stop=0;

    my_queue = new counted_queue<counted_string>;
  }

  void Repeat::add_multip(long len,char *qtr,long n){
    char *rtr;
    long ui,m;
    punct(len,qtr);
    convert(qtr,len);
    phrase_stem();
    for(m=0;m<cnt;m++){
      rtr=list[m];

      ui=0;
      while(*(rtr+ui)!='\0'){
	pct->add_count(rtr+ui,n);
	ui++;
	while((*(rtr+ui)!=' ')&&(*(rtr+ui)!='\0'))ui++;
	if(*(rtr+ui)==' ')ui++;
      }
    }
    clear_list();
  }

  void Repeat::process2(void){
    long ui,len,m,n;
    char *utr;
    pct->node_first();
    if(pct->node_next()){
      utr=pct->show_str();

      if((m=pct->count())>1)lst->add_key(utr);
      while(pct->node_next()){
	char *rtr=pct->show_str();

	if((n=pct->count())>1)lst->add_key(rtr);
	if((m==1)&&(n==1)){
	  len=strlen(utr);
	  ui=strlen(rtr);
	  len=(len<ui)?len:ui;
	  ui=0;
	  while((ui<len)&&(*(utr+ui)==*(rtr+ui)))ui++;
	  if( (0 < ui && ui<len) || // part of utr matched
	     ( (ui == len)  && (*(rtr+ui) != ' ')) // utr ended middle of rtr word
	     ) {
	    --ui;
	    while((0<ui)&&(*(utr+ui)!=' '))ui--;
	    *(utr+ui)='\0';
	  }

	  if(ui>0)lst->add_key(utr);

	}
	utr=rtr;
	m=n;
      }
    }
  }

  
  void Repeat::process( int N){
    my_queue->resize(N);

    pct->node_first();
    while(pct->node_next()){
      
      if ( my_queue->full() )
	my_queue->pop_front();

      my_queue->push_back( pct );

      // consider possible output
      
      while ( my_queue->count() >= N ) {

	if ( my_queue->size() == 1 ) {
	  // only one phrase,automaticly add to list
	  lst->add_key( my_queue->front().strp );
	}

	else {
	 
	  // checking for matching prefix
	 
	  char* frontp = my_queue->front().strp;
	  char* backp = my_queue->back().strp;

	  int front_len = strlen( frontp );
	  int back_len = strlen( backp );
	  // shortest string
	  int min_len = front_len < back_len ? front_len : back_len;
	   
	  int ci = 0;		// index of character 
	  // scan matching strings
	  // ci points to first difference or NULL on short string
	  while((ci<min_len)&&(*(frontp+ci)==*(backp+ci)))ci++;

	  if ( ( 0 < ci && ci < min_len ) || // part of first matched
	       // first ended middle of word in last
	      ( ci == min_len && *(backp+ci) != ' ' ) )
	  {
	    --ci;		// back to where the strings are equal
	    while((0<ci)&&(*(frontp+ci)!=' '))ci--; // to word break
	    *(frontp+ci)='\0';
	  }
	  if(ci)lst->add_key(frontp);
	}

	// remove first word from list
	my_queue->pop_front();

      }	// my_queue->count() >= N
    }
  }


} // namespace iret

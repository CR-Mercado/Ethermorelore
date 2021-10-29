This folder contains 8 files related to the Ethermore NFT. 

Social Media Data 
	- Twitter Follower count over time from social blade. Not every day has a timestamp.
	- Raw followers from Twitter. Ethermore's followers by screenname and user ID, including how many followers they each had at the time stamp.
	- Primary tweets up to timestamp. Retweets, Quote Tweets, and Replies excluded. Includes tweet text, # of likes, # of retweets, and various related calculations over time.

Community Data
	- nft owners. Who owns each of the 15,000 Ethermore NFTs (not aggregated).
	- Owned per Owner. The number of Ethermore NFTs owned by each Ethermore NFT owner.
	- Owner_nfts. A list contained within an RDS file showing which NFTs are owned by those that own Ethermore NFTs (NFT co-occurence).
	

Sales Data
	- The historical sales of Ethermore NFTs from FLipSide Crypto at the block level.
	- Sales with followers. A merge of follower count each day alongside number of sales on that data

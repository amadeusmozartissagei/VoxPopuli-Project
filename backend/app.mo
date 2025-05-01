import LLM "mo:llm";
import Text "mo:base/Text";
import Time "mo:base/Time";
import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Hash "mo:base/Hash";
import Error "mo:base/Error";
import Blob "mo:base/Blob";
import List "mo:base/List";
import Principal "mo:base/Principal";
import Array "mo:base/Array"; 
import Option "mo:base/Option";

actor AnonymousOpinions {
  // Type for storing media
  type MediaType = {
    #image;
    #video;
  };

  type Media = {
    mediaType: MediaType;
    content: Blob;
  };

  // Type for storing votes
  type Vote = {
    #up;
    #down;
  };

  // Type for storing opinions with votes and replies
  type Opinion = {
    id: Nat;
    content: Text;
    timestamp: Time.Time;
    media: ?Media;
    upvotes: Nat;
    downvotes: Nat;
    parentId: ?Nat; // Optional parent ID for replies
  };

  // Type for tracking user votes
  type UserVote = {
    opinionId: Nat;
    vote: Vote;
  };

  // Stable variables for persistence across upgrades
  stable var nextId: Nat = 0;
  stable var opinionsEntries: [(Nat, Opinion)] = [];
  stable var userVotesEntries: [(Principal, [UserVote])] = [];
  
  // Use HashMap for more efficient lookups by ID
  let opinions = HashMap.fromIter<Nat, Opinion>(
    opinionsEntries.vals(), 
    10, 
    Nat.equal, 
    Hash.hash
  );

  // Track user votes to prevent multiple votes on the same opinion
  let userVotes = HashMap.fromIter<Principal, [UserVote]>(
    userVotesEntries.vals(),
    10,
    Principal.equal,
    Principal.hash
  );

  // LLM configuration
  let defaultPrompt = "analisa text ini, dan berikan output 1 jika termasuk hinaan dan berikan output 0 jika tidak terindikasi hinaan. hanya beri saya input 0 atau 1.";
  let MAX_INPUT_LENGTH = 280;

  // Check content using LLM
  public func checkContentWithLLM(content: Text) : async Bool {
    if (Text.size(content) > MAX_INPUT_LENGTH) {
      throw Error.reject("Error: Input exceeds 280 character limit");
    };
    
    let fullPrompt = defaultPrompt # " " # content;
    let result = await LLM.prompt(#Llama3_1_8B, fullPrompt);
    
    // Check if result contains "1" which indicates inappropriate content
    return Text.contains(result, #text "1");
  };

  // Simple filter function as backup
  private func containsInappropriateContent(text: Text) : Bool {
    let lowercaseText = Text.toLowercase(text);
    
    // Define a list of inappropriate words to filter
    let inappropriateWords = ["bajingan", "asu", "kontol"];
    
    for (word in inappropriateWords.vals()) {
      if (Text.contains(lowercaseText, #text word)) {
        return true;
      };
    };
    
    return false;
  };

  // Modified postOpinion function with LLM content moderation
  public shared(msg) func postOpinion(content: Text, media: ?Media, parentId: ?Nat) : async Nat {
    if (Text.size(content) == 0) {
      throw Error.reject("Opinion content cannot be empty");
    };
    
    // First check with simple filter
    if (containsInappropriateContent(content)) {
      throw Error.reject("Opinion contains inappropriate content");
    };
    
    // Then check with LLM for more advanced detection
    let isInappropriate = await checkContentWithLLM(content);
    if (isInappropriate) {
      throw Error.reject("Opinion contains inappropriate content detected by AI");
    };
    
    // If this is a reply, verify parent exists
    switch (parentId) {
      case (?pid) {
        switch (opinions.get(pid)) {
          case null {
            throw Error.reject("Parent opinion does not exist");
          };
          case _ {};
        };
      };
      case null {};
    };
    
    let newOpinion: Opinion = {
      id = nextId;
      content = content;
      timestamp = Time.now();
      media = media;
      upvotes = 0;
      downvotes = 0;
      parentId = parentId;
    };
    
    opinions.put(nextId, newOpinion);
    nextId += 1;
    return newOpinion.id;
  };

  // Helper function to find a vote in a user's vote list
  private func findVote(votes: [UserVote], opinionId: Nat) : ?(UserVote, Nat) {
    var index = 0;
    for (vote in votes.vals()) {
      if (vote.opinionId == opinionId) {
        return ?(vote, index);
      };
      index += 1;
    };
    null
  };

  // Improved Vote on an opinion (upvote or downvote)
  public shared(msg) func voteOnOpinion(opinionId: Nat, vote: Vote) : async () {
    let caller = msg.caller;
    
    // Check if opinion exists
    switch (opinions.get(opinionId)) {
      case null {
        throw Error.reject("Opinion does not exist");
      };
      case (?opinion) {
        // Get user's existing votes
        let userVotesList = switch (userVotes.get(caller)) {
          case null { [] };
          case (?votes) { votes };
        };
        
        // Find existing vote for this opinion
        let existingVoteResult = findVote(userVotesList, opinionId);
        
        // Updated opinion with modified vote counts
        var updatedOpinion = opinion;
        
        switch (existingVoteResult) {
          case null {
            // New vote: increment appropriate counter
            updatedOpinion := switch (vote) {
              case (#up) { 
                { opinion with upvotes = opinion.upvotes + 1 } 
              };
              case (#down) { 
                { opinion with downvotes = opinion.downvotes + 1 } 
              };
            };
            
            // Add new vote to user's vote list
            let updatedVotes = Array.append(
              userVotesList, 
              [{ opinionId = opinionId; vote = vote }]
            );
            userVotes.put(caller, updatedVotes);
          };
          case (?(existingVote, index)) {
            // If vote is the same, do nothing
            if (existingVote.vote == vote) return;
            
            // Change vote: adjust counters
            updatedOpinion := switch (existingVote.vote, vote) {
              case (#up, #down) { 
                { opinion with 
                  upvotes = opinion.upvotes - 1; 
                  downvotes = opinion.downvotes + 1 
                } 
              };
              case (#down, #up) { 
                { opinion with 
                  upvotes = opinion.upvotes + 1; 
                  downvotes = opinion.downvotes - 1 
                } 
              };
              case _ { opinion }; // Impossible case, but needed for exhaustiveness
            };
            
            // Update user's vote
            let updatedVotes = Array.tabulate<UserVote>(
              userVotesList.size(), 
              func(i: Nat) : UserVote { 
                if (i == index) {
                  {
                    opinionId = opinionId;
                    vote = vote;
                  }
                } else {
                  userVotesList[i] 
                }
              }
            );
            userVotes.put(caller, updatedVotes);
          };
        };
        
        // Update the opinion in the HashMap
        opinions.put(opinionId, updatedOpinion);
      };
    };
  };

  // Query function to get all top-level opinions (not replies)
  public query func getAllOpinions() : async [Opinion] {
    Iter.toArray(
      Iter.filter(
        opinions.vals(), 
        func (opinion: Opinion): Bool { 
          switch (opinion.parentId) {
            case null { true };
            case _ { false };
          }
        }
      )
    );
  };

  // Query function to get an opinion by ID
  public query func getOpinion(id: Nat) : async ?Opinion {
    opinions.get(id);
  };
  
  // Get replies to a specific opinion
  public query func getReplies(opinionId: Nat) : async [Opinion] {
    Iter.toArray(
      Iter.filter(
        opinions.vals(),
        func (opinion: Opinion): Bool {
          switch (opinion.parentId) {
            case (?pid) { pid == opinionId };
            case null { false };
          }
        }
      )
    );
  };
  
  // Get a user's vote on a specific opinion
  public query(msg) func getUserVote(opinionId: Nat) : async ?Vote {
    let caller = msg.caller;
    
    switch (userVotes.get(caller)) {
      case null { null };
      case (?votes) {
        for (vote in votes.vals()) {
          if (vote.opinionId == opinionId) {
            return ?vote.vote;
          };
        };
        null;
      };
    };
  };
  
  // System functions for data persistence
  system func preupgrade() {
    opinionsEntries := Iter.toArray(opinions.entries());
    userVotesEntries := Iter.toArray(userVotes.entries());
  };

  system func postupgrade() {
    opinionsEntries := [];
    userVotesEntries := [];
  };
}

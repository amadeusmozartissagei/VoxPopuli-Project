import LLM "mo:llm";
import Text "mo:base/Text";
import Time "mo:base/Time";
import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Hash "mo:base/Hash";
import Error "mo:base/Error";
import Blob "mo:base/Blob";
import Principal "mo:base/Principal";
import Array "mo:base/Array"; 
import Buffer "mo:base/Buffer";

actor AnonymousOpinions {
  // Consolidated type definitions
  type MediaType = { #image; #video };
  type Media = { mediaType: MediaType; content: Blob };
  type Vote = { #up; #down };
  
  type UserPointsTracker = {
    totalPoints: Nat;
    remainingPoints: Nat;
  };
  
  type Opinion = {
    id: Nat;
    content: Text;
    // author: Principal;
    timestamp: Time.Time;
    media: ?Media;
    upvotes: Nat;
    downvotes: Nat;
    parentId: ?Nat;
  };
  
  type UserVote = {
    opinionId: Nat;
    vote: Vote;
  };

  // Optimized constants
  let CONFIG = {
    POST_POINTS = { cost = 7; min = 7 };
    REPLY_POINTS = { earned = 5; cost = 0 };
    DEFAULT_INITIAL_POINTS : Nat = 50;
    MAX_INPUT_LENGTH : Nat = 280;
    MAX_REPLY_DEPTH : Nat = 2;
  };

  // Efficient data structures
  let opinions = HashMap.HashMap<Nat, Opinion>(10, Nat.equal, Hash.hash);
  let userVotes = HashMap.HashMap<Principal, [UserVote]>(10, Principal.equal, Principal.hash);
  let userPoints = HashMap.HashMap<Principal, UserPointsTracker>(10, Principal.equal, Principal.hash);

  // Persistent state variables
  stable var nextId: Nat = 0;
  stable var opinionsEntries: [(Nat, Opinion)] = [];
  stable var userVotesEntries: [(Principal, [UserVote])] = [];
  stable var userPointsEntries: [(Principal, UserPointsTracker)] = [];

  // Optimized LLM content moderation
  let defaultPrompt = "analisa text ini, dan berikan output 1 jika termasuk hinaan dan berikan output 0 jika tidak terindikasi hinaan. hanya beri saya input 0 atau 1.";

  // Efficient initialization of user points
  private func initUserPoints(principal: Principal) : UserPointsTracker {
    let pointsTracker : UserPointsTracker = {
      totalPoints = CONFIG.DEFAULT_INITIAL_POINTS;
      remainingPoints = CONFIG.DEFAULT_INITIAL_POINTS;
    };
    userPoints.put(principal, pointsTracker);
    pointsTracker
  };

  // Combined content moderation function
  private func moderateContent(content: Text) : async Bool {
    if (Text.size(content) > CONFIG.MAX_INPUT_LENGTH) {
      throw Error.reject("Input exceeds character limit");
    };

    // Simple word filter
    let inappropriateWords = ["bajingan", "asu", "kontol"];
    let lowercaseContent = Text.toLowercase(content);
    
    if (Array.filter<Text>(inappropriateWords, func(word: Text) : Bool { 
      Text.contains(lowercaseContent, #text word) 
    }).size() > 0) {
      return true;
    };

    // LLM moderation (with optional error handling)
    try {
      let fullPrompt = defaultPrompt # " " # content;
      let result = await LLM.prompt(#Llama3_1_8B, fullPrompt);
      
      return Text.contains(result, #text "1");
    } catch (e) {
      // Fallback to simple word filter if LLM fails
      return false;
    };
  };

  // Centralized points management
  private func updatePoints(
    principal: Principal, 
    earned: Nat, 
    spent: Nat
  ) : UserPointsTracker {
    let currentTracker = switch (userPoints.get(principal)) {
      case null { initUserPoints(principal) };
      case (?tracker) { tracker };
    };

    let updatedTracker : UserPointsTracker = {
      totalPoints = currentTracker.totalPoints + earned - spent;
      remainingPoints = currentTracker.remainingPoints + earned - spent;
    };

    userPoints.put(principal, updatedTracker);
    updatedTracker
  };

  // Advanced opinion posting with comprehensive checks
  public shared(msg) func postOpinion(
    content: Text, 
    media: ?Media, 
    parentId: ?Nat
  ) : async Nat {
    let caller = msg.caller;
    
    // Comprehensive validation
    if (Text.size(content) == 0) {
      throw Error.reject("Opinion content cannot be empty");
    };

    // Points and permission check
    let pointsTracker = updatePoints(caller, 0, CONFIG.POST_POINTS.cost);
    if (pointsTracker.remainingPoints < CONFIG.POST_POINTS.min) {
      throw Error.reject("Insufficient points to post opinion");
    };

    // Content moderation
    let isInappropriate = await moderateContent(content);
    if (isInappropriate) {
      throw Error.reject("Inappropriate content detected");
    };

    // Optional parent opinion validation
    switch (parentId) {
      case (?pid) {
        let ?parentOpinion = opinions.get(pid) else throw Error.reject("Invalid parent opinion");
        
        // Check reply depth
        if (parentOpinion.parentId != null and parentOpinion.parentId != null) {
          throw Error.reject("Maximum reply depth reached");
        };
      };
      case null {};
    };
    
    let newOpinion: Opinion = {
      id = nextId;
      content = content;
      // author = caller;
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

  // Enhanced voting mechanism
  public shared(msg) func voteOnOpinion(opinionId: Nat, vote: Vote) : async () {
    let caller = msg.caller;
    let opinion = switch (opinions.get(opinionId)) {
      case null { throw Error.reject("Opinion not found") };
      case (?op) { op };
    };

    let userVotesList = switch (userVotes.get(caller)) {
      case null { [] };
      case (?votes) { votes };
    };

    let (updatedOpinion, updatedVotes) = switch (
      Array.find(userVotesList, func(v: UserVote) : Bool { v.opinionId == opinionId })
    ) {
      case null {
        // New vote
        let newVote = { opinionId; vote };
        let newOpinion = switch (vote) {
          case (#up) { { opinion with upvotes = opinion.upvotes + 1 } };
          case (#down) { { opinion with downvotes = opinion.downvotes + 1 } };
        };
        (newOpinion, Array.append(userVotesList, [newVote]))
      };
      case (?existingVote) {
        // Vote change logic
        let newOpinion = switch (existingVote.vote, vote) {
          case (#up, #down) { 
            { opinion with upvotes = opinion.upvotes - 1; downvotes = opinion.downvotes + 1 } 
          };
          case (#down, #up) { 
            { opinion with upvotes = opinion.upvotes + 1; downvotes = opinion.downvotes - 1 } 
          };
          case _ { opinion };
        };
        
        let updatedVotes = Array.map(userVotesList, func(v: UserVote) : UserVote {
          if (v.opinionId == opinionId) { { opinionId; vote } } else { v }
        });
        
        (newOpinion, updatedVotes)
      };
    };

    opinions.put(opinionId, updatedOpinion);
    userVotes.put(caller, updatedVotes);
  };

  // Advanced querying functions
  public query func getOpinionThread(opinionId: Nat) : async {
    opinion: ?Opinion;
    replies: [Opinion];
  } {
    {
      opinion = opinions.get(opinionId);
      replies = Iter.toArray(Iter.filter(
        opinions.vals(),
        func(op: Opinion) : Bool { 
          op.parentId == ?opinionId 
        }
      ));
    }
  };

  // User points retrieval
  public query func getUserPoints(principal: Principal) : async ?UserPointsTracker {
    userPoints.get(principal)
  };

  // System upgrade management
  system func preupgrade() {
    opinionsEntries := Iter.toArray(opinions.entries());
    userVotesEntries := Iter.toArray(userVotes.entries());
    userPointsEntries := Iter.toArray(userPoints.entries());
  };

  system func postupgrade() {
    for ((id, opinion) in opinionsEntries.vals()) {
      opinions.put(id, opinion);
    };

    for ((principal, votes) in userVotesEntries.vals()) {
      userVotes.put(principal, votes);
    };

    for ((principal, pointsTracker) in userPointsEntries.vals()) {
      userPoints.put(principal, pointsTracker);
    };

    opinionsEntries := [];
    userVotesEntries := [];
    userPointsEntries := [];
  };
}
